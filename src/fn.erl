%% Copyright (c) 2013 Ulf Wiger <ulf@wiger.net>
%% License: See attached LICENSE file
-module(fn).

-export([join/1, join/2,
	 split/1,
	 dir/1,
	 base/1, base/2]).

join(L) when is_list(L) ->
    filename:join(L).

join(A, B) ->
    filename:join(A, B).

split(F) ->
    filename:split(F).

dir(F) ->
    filename:dirname(F).

base(F) ->
    filename:basename(F).

base(F, Ext) ->
    filename:basename(F, Ext).
