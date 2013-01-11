%% Copyright (c) 2013 Ulf Wiger <ulf@wiger.net>
%% License: See attached LICENSE file
-module(f).

-export([ls/1,
	 rm/1,
	 mkdir/1,
	 rmdir/1,
	 script/1, script/2,
	 consult/1,
	 pconsult/2,
	 pscript/2, pscript/3]).

ls(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} -> Files;
	{error, E}  -> error(E, [Dir])
    end.

rm(File) ->
    case file:delete(File) of
	ok -> ok;
	{error, enoent} ->  ok;
	{error, E}      ->  error(E, [File])
    end.

mkdir(Dir) ->
    case file:make_dir(Dir) of
	ok -> ok;
	{error, E} -> error(E, [Dir])
    end.

rmdir(Dir) ->
    case file:del_dir(Dir) of
	ok -> ok;
	{error, E} -> error(E, [Dir])
    end.

consult(F) ->
    case file:consult(F) of
	{ok, Terms} -> Terms;
	{error, E}  -> error(E, [F])
    end.

pconsult(Path, F) ->
    case file:path_consult(Path, F) of
	{ok, Terms, _} -> Terms;
	{error, E}     -> error(E, [F])
    end.

script(F) ->
    case file:script(F, e:bs([{'SCRIPT', F}])) of
	{ok, Result} -> Result;
	{error, E}   -> error(E, [F])
    end.

script(F, Bs) ->
    case file:script(F, e:b('SCRIPT', F, erl_eval:del_binding('SCRIPT', Bs))) of
	{ok, Result} -> Result;
	{error, E}   -> error(E, [F, Bs])
    end.

pscript(Path, F) ->
    pscript(Path, F, e:bs()).

pscript(Path, F, Bs) ->
    case file:path_open(Path, F, [read]) of
	{ok, Fd, Filename} ->
	    file:close(Fd),
	    Bs1 = e:b('SCRIPT', Filename,
		      erl_eval:del_binding('SCRIPT', Bs)),
	    case file:script(Filename, Bs1) of
		{ok, Result} -> Result;
		{error, E}      -> error(E, [Path, F, Bs1])
	    end
    end.
