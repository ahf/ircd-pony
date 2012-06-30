-module(ircd_pony).


-export([start/0]).

dependent_apps() ->
    {ok, Apps} = application:get_key(admanager, applications),
    Apps -- [kernel, stdlib].

start() ->
    application:load(ircd_pony),
    [ ensure_started(A) || A <- dependent_apps() ],
    application:start(ircd_pony).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error,{already_started, App}} -> ok
    end.

