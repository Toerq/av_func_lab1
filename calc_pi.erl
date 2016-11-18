-module(calc_pi).
-export([calc/2]).

calc(N, Schedulers) ->
    Pid = self(),
    [spawn_link(fun() ->
			worker(round(N/Schedulers), Pid)
		end)
     || _ <- lists:seq(1,Schedulers - 1)],
    
    worker(round(N/Schedulers) + N rem Schedulers, Pid),
    receiveLoop(Schedulers, 0, N).
%    (lists:sum([receive
%	 Count -> Count
%     end
%     || _ <- lists:seq(1,Schedulers)])/N)*4.

receiveLoop(0, Sum, N) ->
    (Sum/N)*4;
receiveLoop(Schedulers, Sum, N) ->
    receive
	{success, NewSum} -> receiveLoop(Schedulers - 1, Sum + NewSum, N);
	Error ->
	    io:format("Unknown error: ~w~n", [Error]),
	    receiveLoop(Schedulers - 1, Sum, N)
     end.


%worker(Points, Pid) ->
 %   Pid ! length([X || X <- lists:seq(1,Points), inUnitCircle(rand:uniform(),rand:uniform())]).

worker(Points, Pid) ->
    worker(Points, Pid, 0).
worker(0, Pid, Sum) ->
    Pid ! {success, Sum};
worker(Points, Pid, Sum) ->
    case inUnitCircle(rand:uniform(),rand:uniform()) of
	true ->
	    worker(Points - 1, Pid, Sum + 1);
	false ->
	    worker(Points - 1, Pid, Sum)
    end.

inUnitCircle(X,Y) ->
    (math:pow((X),2) + math:pow((Y),2)) < 1.
    
    

%{"init terminating in do_boot",
%{badarith,
%[{calc_pi,receiveLoop,3,
%[{file,"calc_pi.erl"},
%{line,22}]},
%{timer,tc,3,[{file,"timer.erl"},
%{line,197}]},
%{calc_pi_grader,'-calc_sample_grader/0-lc$^0/1-0-',1,[{file,"calc_pi_grader.erl"},
%{line,31}]},
%{calc_pi_grader,'-calc_sample_grader/0-lc$^0/1-0-',1,[{file,"calc_pi_grader.erl"},
%{line,34}]},
%{calc_pi_grader,calc_sample_grader,0,[{file,"calc_pi_grader.erl"},
%{line,28}]},
%{init,start_em,1,[]},
%{init,do_boot,3,[]}]}}
    
    
