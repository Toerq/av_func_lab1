%%%-------------------------------------------------------------------
%%% @author Christian <toernqvist@gmail.com>
%%% @doc RPC over TCP server. This module defines a server process that
%%%
%%% listens for incoming TCP connections and allows the user to
%%%
%%% execute RPC commands via that TCP stream.
%%% @end
%%%-------------------------------------------------------------------

-module(vector_server).
-behaviour(gen_server).
-compile([export_all]).
%% API 
-export([
start_link/1,
start_link/0,
stop/0
]).

%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1056).

-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%--------------------------------------------------------------------


start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%% Count = integer()
%% @end
%%--------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).
%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


compute() -> 
    gen_server:call(?SERVER, compute).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]), %Creates TCP listening port
    {ok, #state{port = Port, lsock = LSock}, 0}. % Timeout = 0

% invoked with gen_server:call/2
handle_call(get_count, _From, State) -> % Sync request>
    {reply, {ok, State#state.request_count}, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    Reply = helloeveryboddy,
    {reply, Reply, State}.


handle_cast(stop, State) -> % Async request>
    {stop, normal, State};
handle_cast(compute, State) -> % Async request>
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


% Handles normal ! messages, "out-of-band messages"
handle_info({tcp, Socket, RawData}, State) -> %RawData = ascii data text client sen>
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};

handle_info(_Info, State) ->
%    {port, lsock, request_count = 0}).
    LSock = State#state.lsock,
    gen_tcp:accept(LSock),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
do_rpc(Socket, RawData) ->
    try
	M = ?SERVER,
	F = compute,
	[A] = args_to_terms(RawData),
	Result = apply(?SERVER, eval, [A]),
	gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [Result]))
    catch
	_Class:Err ->
	    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

eval(X) ->
    try eval(X, 100) of
	Res -> Res
    catch
	Throw -> Throw
    end.

eval(_, 0) ->
    throw(nesting_depth_greater_than_100);

eval({Type, Expr1, Expr2}, Depth) ->
    Value1 = eval(Expr1, Depth - 1),
    Value2 = eval(Expr2, Depth - 1),

    case is_list(Value2) of
	true -> ok;
	false -> throw(second_expr_not_list)
    end,

    case (Type == 'dot') or (Type == 'sub') or (Type == 'add') of
	true -> % Vector op
	    Len1 = length(Value1),
	    Len2 = length(Value2),
	    case (Len1 /= Len2) of
		true -> throw(bad_length);
		false -> ok
	    end,
	
	    case Type of
		'dot' ->
		    [lists:nth(I, Value1)*lists:nth(I, Value2) || I <- lists:seq(1,Len1)];
		'add' ->
		    [lists:nth(I, Value1)+lists:nth(I, Value2) || I <- lists:seq(1,Len1)];
		'sub' ->
		    [lists:nth(I, Value1)-lists:nth(I, Value2) || I <- lists:seq(1,Len1)]
	    end;

	false -> % Scalar op
	    IntValue = eval(Expr1, Depth - 1),
	    case is_integer(IntValue) of
		true ->
		    case Type of
			'mul' -> % norm one
			    [lists:nth(I, Value2)*IntValue || I <- lists:seq(1,length(Expr2))];
			'div' ->
			    case IntValue of
				0 -> 
				    throw(div_by_zero);
				IntValue -> 
				    [lists:nth(I, Value2)/IntValue || I <- lists:seq(1,length(Expr2))]
			    end
		    end;
		false ->
		    throw(notInt)
	    end
    end;

eval({ScalarOp, Expr}, Depth) ->
    case ScalarOp of
	'norm_one' ->
	    norm_one(eval(Expr, Depth - 1));
	'norm_inf' ->
	    norm_inf(eval(Expr, Depth - 1))
    end;

eval(Value, _) ->
    case is_list(Value) of
	true ->
	    case (0 < length(Value)) and (length(Value) < 101) of 
		true ->
		    Value;
		false ->
		    throw(unsupported_vector_size)
	    end;
	false ->
	    Value  
    end.

norm_one(X) ->
    lists:sum([abs(Y) || Y <- X]).

norm_inf(X) ->
    lists:max(X).
    
