-module(calc_pi).
-export([calc/2]).

calc(N, Schedulers) ->
    Pid = self(),
    [spawn_link(fun() ->
			worker(round(N/Schedulers), Pid)
		end)
     || _ <- lists:seq(1,Schedulers - 1)],
    
    worker(round(N/Schedulers) + N rem Schedulers, Pid),
    
    (lists:sum([receive
	 Count -> Count
     end
     || _ <- lists:seq(1,Schedulers)])/N)*4.

worker(Points, Pid) ->
    Pid ! length([X || X <- lists:seq(1,Points), inUnitCircle(random:uniform(),random:uniform())]).

inUnitCircle(X,Y) ->
    (math:pow((X),2) + math:pow((Y),2)) < 1.
    
    

    
    