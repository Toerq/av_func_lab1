-module(calc_pi).
-export([calc/2]).

calc(N, Schedulers) ->
    Ref = make_ref(),
    Pid = self(),
    [spawn_link(fun() ->
			worker(round(N/Schedulers), Pid, Ref)
		end)
     || _ <- lists:seq(1,Schedulers - 1)],
  
%    spawn_link(fun() -> 
%		       worker(round(N/Schedulers) + N rem Schedulers, Pid, Ref)
%	       end),
    worker(round(N/Schedulers) + N rem Schedulers, Pid, Ref),
    receiveLoop(Schedulers, N, Ref).

receiveLoop(Schedulers, N, Ref) ->
    receiveLoop(Schedulers, 0, N, 0, Ref).
receiveLoop(0, Sum, N, _, _) ->
    (Sum/N)*4;
receiveLoop(Schedulers, Sum, N, PrevValue, Ref) ->
    receive
	{success, Ref, NewSum} -> 
	    receiveLoop(Schedulers - 1, Sum + NewSum, N, NewSum, Ref);
	Error ->
	    io:format("Unknown error: ~w~n", [Error]),
	    receiveLoop(Schedulers - 1, Sum + PrevValue, N, PrevValue, Ref)
     end.

worker(Points, Pid, Ref) ->
    worker(Points, Pid, 0, Ref).
worker(0, Pid, Sum, Ref) ->
    Pid ! {success, Ref, Sum};
worker(Points, Pid, Sum, Ref) ->
    case inUnitCircle() of
	true ->
	    worker(Points - 1, Pid, Sum + 1, Ref);
	false ->
	    worker(Points - 1, Pid, Sum, Ref)
    end.

inUnitCircle() ->
    (math:pow((rand:uniform()),2) + math:pow((rand:uniform()),2)) < 1.
