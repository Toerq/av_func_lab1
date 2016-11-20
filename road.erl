-module(road).
-export([days/3]).
-compile([export_all]).

-include_lib("/home/chto2175/proper/include/proper.hrl").
-include_lib("/home/chto2175/eunit/include/eunit.hrl").

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
	    unconstructed(Overlap, Unconstructed,  [U] ++ NewSegments);
	covered ->
	    unconstructed(Overlap, Unconstructed, NewSegments);
	NewSegment ->
	    unconstructed(Overlap, Unconstructed, NewSegment ++ NewSegments)
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
    case (StartOverlap > Start) and (Start < EndOverlap) and (Start < EndOverlap) and (EndOverlap < End)  of
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

ascendingTuples([]) ->
    true;
ascendingTuples([{V1,V2}|T]) ->
    case V2 > V1 of
	true ->
	    ascendingTuples(T);
	false ->
	    false
    end.

noneInside([],_) ->
    true;
noneInside([{V1,V2}|T], {O1,O2}) ->
    case not ((V1 < O1) and (V2 > O1) or (V1 < O1) and (V2 > O2)) of
	true ->
	    noneInside(T, {O1,O2});
	false ->
	    false
    end.

segmentCount([], _, Count) ->
    Count;
segmentCount([T|Ts], X, Count) ->
    case overlap(T, X) of
	[] ->
	    segmentCount(Ts, X, Count + 1);
	covered ->
	    segmentCount(Ts, X, Count);
	[_,_] ->
	    segmentCount(Ts, X, Count + 2);
	[_] ->
	    segmentCount(Ts, X, Count + 1);
	Any ->
	    io:format("Error: ~w~n", [Any])
    end.

prop_overlap() ->
    ?FORALL({L,T}, {list(tuple([integer(),integer()])), tuple([integer(),integer()])},
	    ?IMPLIES(ascendingTuples(L) and ascendingTuples([T]) and (length(L) > 0), 
		     length(unconstructed(T, L, [])) == segmentCount(L,T,0))).

prop_newListSize() ->
    ?FORALL({L,T}, {list(tuple([integer(),integer()])), tuple([integer(),integer()])},
	    ?IMPLIES(ascendingTuples(L) and ascendingTuples([T]), 
		     noneInside(unconstructed(T, L, []),T))).


overlap_test_() ->
    [test_zero(), test_one(), test_two(), test_three(), test_four(), test_five(), test_six(), test_seven(), test_eight(), test_nine(), test_ten()].

test_zero() ->    
    ?_assertEqual([], overlap({10,20}, {0,5})).
test_one() ->    
    ?_assertEqual([{15,20}], overlap({10,20}, {0,15})).
test_two() ->    
    ?_assertEqual([{10,12}, {16,20}], overlap({10,20}, {12,16})).
test_three() ->    
    ?_assertEqual([{10,16}], overlap({10,20}, {16,21})).
test_four() ->    
    ?_assertEqual([], overlap({10,20}, {22,25})).
test_five() ->    
    ?_assertEqual(covered, overlap({10,20}, {0,30})).

test_six() ->
    ?_assertEqual(15, maxLength([{0,5}, {5,20}, {30,31}], 0)).

test_seven() ->
    ?_assertEqual([{20,30},{1,5}], unconstructed({5,20}, [{1,10}, {20,30}], [])).

test_eight() ->
    ?_assertEqual(2, days(30, [{1,5},{11,27},{2,14},{18,28}],6)).

test_nine() ->
    ?_assertEqual(-1, days(30, [{1,5},{11,27},{2,14},{18,28}],1)). 

test_ten() ->
    ?_assertEqual([{-1,0}], overlap({-10,0}, {-10,-1})).
