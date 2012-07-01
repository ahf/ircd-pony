-module(pony).

-export([start/0, me/0, description/0, server/0, version/0]).

start() ->
    application:load(pony_core),
    [ ensure_started(A) || A <- dependent_apps() ],
    application:start(pony_core).

%% @todo Raise this to the configuration file
me() ->
    {ok, N} = application:get_env(pony_core, host),
    N.

description() ->
    {ok, N} = application:get_env(pony_core, description),
    N.

version() ->
    {ok, V} = application:get_key(pony_core, vsn),
    V.

server() ->
    io_lib:format("~s[todo]", [me()]).

%% ----------------------------------------------------------------------
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error,{already_started, App}} -> ok
    end.

dependent_apps() ->
    {ok, Apps} = application:get_key(pony_core, applications),
    Apps -- [kernel, stdlib].

