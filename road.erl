-module(road).
-export([days/3]).
-compile([export_all]).

days(L, Segments, X) ->
    Unconstructed = [{1, L}],
    days(L, Segments, X, Unconstructed, L).

days(_, [], X, _, MaxLength) ->
    case MaxLength > X of
	true -> -1;
	false -> MaxLength
    end;

days(L, [Segment|Segments], X, Unconstructed, _) ->
    NewUnconstructed = unconstructed(Segment, Unconstructed, []),
    NewMaxLength = maxLength(NewUnconstructed, 0),
    days(L, Segments, X, NewUnconstructed, NewMaxLength).

unconstructed(_, [], NewSegments) ->
    NewSegments;
unconstructed(Overlap, [U|Unconstructed], NewSegments) ->
    NewSegment = overlap(U, Overlap),
    case NewSegment of
	[] -> 
	    unconstructed(Overlap, Unconstructed,  NewSegments ++ [U]);
	covered ->
	    unconstructed(Overlap, Unconstructed, NewSegments);
	NewSegment ->
	    unconstructed(Overlap, Unconstructed, NewSegments ++ NewSegment)
    end.

maxLength([], MaxLength) ->
    MaxLength;
maxLength([{X,Y}|Unconstructed], MaxLength) ->
    case Y - X > MaxLength of
	true ->
	    maxLength(Unconstructed, Y - X);
	false ->
	    maxLength(Unconstructed, MaxLength)
    end.

overlap({Start, End}, {StartOverlap, EndOverlap}) ->
    case (StartOverlap =< Start) and (Start =< EndOverlap) and (End >= EndOverlap) of
	true ->
	    Return1 = [{EndOverlap, End}];
	false ->
	    Return1 = []
    end,
    case (StartOverlap >= Start) and (Start =< EndOverlap) and (Start =< EndOverlap) and (EndOverlap =< End)  of
	true ->
	    Return2 = [{Start, StartOverlap}, {EndOverlap, End}];
	false ->
	    Return2 = []
    end,
    case (Start =< StartOverlap) and (StartOverlap =< End) and (End =< EndOverlap) of
	true ->
	    Return3 = [{Start, StartOverlap}];
	false ->
	    Return3 = []
    end,
    case (Start >= StartOverlap) and (End =< EndOverlap) of
	true ->
	    Result4 = covered;
	false ->
	    Result4 = []
    end,
    
    case Result4 of
	covered ->
	    covered;
	_ ->
	    Return1 ++ Return2 ++ Return3
    end.

    

%road:days(30, [{1,5},{11,27},{2,14},{18,28}],6).
%2 
%road:days(30, [{1,5},{11,27},{2,14},{18,28}],1). 
%-1 
