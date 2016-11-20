-module(bughunt).
-compile([export_all]).
-export([test/1, test_all/0]).
-include_lib("/home/chto2175/proper/include/proper.hrl").
-include_lib("/home/chto2175/eunit/include/eunit.hrl").

prop_add(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'add', L1, L2}) =:= eval_({'add', L1, L2})).
prop_sub(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'sub', L1, L2}) =:= eval_({'sub', L1, L2})).
prop_dot(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'dot', L1, L2}) =:= eval_({'dot', L1, L2})).
prop_mul(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'div', {'norm_one', L1}, L2}) =:= eval_({'div', {'norm_one', L1}, L2})).
prop_div(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'mul', {'norm_one', L1}, L2}) =:= eval_({'mul', {'norm_one', L1}, L2})).
prop_mul2(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'div', {'norm_inf', L1}, L2}) =:= eval_({'div', {'norm_inf', L1}, L2})).
prop_div2(Fun) ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
	    Fun({'mul', {'norm_inf', L1}, L2}) =:= eval_({'mul', {'norm_inf', L1}, L2})).

test_all() ->
    test_all(1, []).

test_all(51, L) ->
    io:format("Not working: ~n"),
    io:format("~w, ", [length(L)]);

test_all(N, L) ->
    io:format("--- Function ~w ---~n", [N]),
    case test(N) of
	correct ->
	    io:format("Working!~n"),
	    test_all(N+1, L);
	Broken ->
	    io:format("Broken!~n"),
	    test_all(N+1,  [{Broken}] ++ L)
    end.

test(I) ->
    Fun = vectors:vector(I),
    case proper:counterexample(prop_add(Fun),[quiet, noshrink]) of 
	true -> L1 = [];
	[{I11, I12}] ->
	    L1 = {{'add', {I11, I12}}, Fun({'add', I11, I12}), eval_({'add', I11, I12}), "The operation add is not supported"}
    end,
    case proper:counterexample(prop_sub(Fun),[quiet, noshrink]) of 
	true -> L2 = [];
	[{I21, I22}] ->
	    L2 = {{'sub', {I21, I22}}, Fun({'sub', I21, I22}),  eval_({'sub', I21, I22}), "The operation sub is not supported"}
    end,
    case proper:counterexample(prop_dot(Fun),[quiet, noshrink]) of 
	true -> L3 = [];
	[{I31, I32}] ->
	    L3 = {{'dot', {I31, I32}}, Fun({'dot', I31, I32}), eval_({'dot', I31, I32}), "The operation dot is not supported"}
    end,
    case proper:counterexample(prop_div(Fun),[quiet, noshrink]) of 
	true -> L4 = [];
	[{I41, I42}] ->
	    L4 = {{'div', {'norm_one',I41}, I42}, Fun({'div', {'norm_one', I41}, I42}), eval_({'div', {'norm_one', I41}, I42}), "The operation div with norm_one is not supported"}
    end,
    case proper:counterexample(prop_mul(Fun),[quiet, noshrink]) of 
	true -> L5 = [];
	[{I51, I52}] -> 
	    L5 = {{'mul', {'norm_one',I51}, I52},  Fun({'mul', {'norm_one', I51}, I52}),  eval_({'mul', {'norm_one', I51}, I52}), "The operation mul with norm_one is not supported"}
    end,
    case proper:counterexample(prop_div2(Fun),[quiet, noshrink]) of 
	true -> L6 = [];
	[{I61, I62}] ->
	    L6 = {{'div', {'norm_inf',I61}, I62},  Fun({'div', {'norm_inf', I61}, I62}),  eval_({'div', {'norm_inf', I61}, I62}), "The operation div with norm_inf is not supported"}
    end,
    case proper:counterexample(prop_mul2(Fun),[quiet, noshrink]) of 
	true ->
	    L7 = [];
	[{I71, I72}] -> 	
	    L7 = {{'mul', {'norm_inf',I71}, I72}, Fun({'mul', {'norm_inf', I71}, I72}),  eval_({'mul', {'norm_inf', I71}, I72}), "The operation mul with norm_inf is not supported"}
    end,

    TempReturn = [L1] ++ [L2] ++ [L3] ++ [L4] ++ [L5] ++ [L6] ++ [L7],
    Return = [X || X <- TempReturn, X /= []],
    case length(Return) > 1 of
	true -> Return;
	false -> correct
    end.

eval_(X) ->
    try eval_(X, 100) of
	Res -> Res
    catch
	_ -> error
    end.

eval_(_, 0) ->
    throw(nesting_depth_greater_than_100);

eval_({Type, Expr1, Expr2}, Depth) ->
    Value1 = eval_(Expr1, Depth - 1),
    Value2 = eval_(Expr2, Depth - 1),

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
	    IntValue = eval_(Expr1, Depth - 1),
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

eval_({ScalarOp, Expr}, Depth) ->
    case ScalarOp of
	'norm_one' ->
	    norm_one(eval_(Expr, Depth - 1));
	'norm_inf' ->
	    norm_inf(eval_(Expr, Depth - 1))
    end;

eval_(Value, _) ->
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
    lists:max([abs(Y) || Y <- X]).
    
