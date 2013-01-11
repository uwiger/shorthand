%% Copyright (c) 2013 Ulf Wiger <ulf@wiger.net>
%% License: See attached LICENSE file
-module(e).

-export([scan/1,
	 parse_term/1,
	 parse_exprs/1,
	 exprs/1,
	 exprs/2,
	 bs/0,
	 bs/1,
	 b/3]).

scan(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} -> Tokens;
	{error, E}      -> error(E, [Str])
    end.

parse_term(Tokens) ->
    case erl_parse:parse_term(ensure_dot(Tokens)) of
	{ok, Term} -> Term;
	{error, E} -> error(E, [Tokens])
    end.

parse_exprs(Tokens) ->
    case erl_parse:parse_exprs(ensure_dot(Tokens)) of
	{ok, Exprs} -> Exprs;
	{error, E}  -> error(E, [Tokens])
    end.

exprs(Exprs) ->
    {value, Value, _NewBindings} = erl_eval:exprs(Exprs, bs()),
    Value.

exprs(Exprs, Bs) ->
    {value, Value, _NewBindings} = erl_eval:exprs(Exprs, Bs),
    Value.

bs() ->
    erl_eval:new_bindings().

bs(KVL) ->
    lists:foldl(fun({K,V}, Acc) ->
			erl_eval:add_binding(K, V, Acc)
		end, bs(), KVL).

b(K, V, Bs) ->
    erl_eval:add_binding(K, V, Bs).

%% helpers

ensure_dot(Tokens) ->
    case lists:reverse(Tokens) of
	[{dot,_}|_] -> Tokens;
	[Last|Tail] -> lists:reverse([{dot,element(2,Last)},Last|Tail])
    end.
