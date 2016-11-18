-module(calc_pi).
-export([calc/2]).

calc(N, Schedulers) ->
    Pid = self(),
    [spawn_link(fun() ->
			worker(round(N/Schedulers), Pid)
		end)
     || _ <- lists:seq(1,Schedulers - 1)],
    
    worker(round(N/Schedulers) + N rem Schedulers, Pid),
    receiveLoop(Schedulers, N).

receiveLoop(Schedulers, N) ->
    receiveLoop(Schedulers, 0, N, 0).
receiveLoop(0, Sum, N, _) ->
    (Sum/N)*4;
receiveLoop(Schedulers, Sum, N, Pred) ->
    receive
	{success, NewSum} -> 
	    receiveLoop(Schedulers - 1, Sum + NewSum, N, NewSum);
	Error ->
	    io:format("Unknown error: ~w~n", [Error]),
	    receiveLoop(Schedulers - 1, Sum + Pred, N, Pred)
     end.

worker(Points, Pid) ->
    worker(Points, Pid, 0).
worker(0, Pid, Sum) ->
    Pid ! {success, Sum};
worker(Points, Pid, Sum) ->
    case inUnitCircle() of
	true ->
	    worker(Points - 1, Pid, Sum + 1);
	false ->
	    worker(Points - 1, Pid, Sum)
    end.

inUnitCircle() ->
    (math:pow((rand:uniform()),2) + math:pow((rand:uniform()),2)) < 1.
