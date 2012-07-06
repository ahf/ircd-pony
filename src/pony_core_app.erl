-module(pony_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(pony_core, port),
    {ok, SSLPort} = application:get_env(pony_core, ssl_port),
    {ok, Pid} = pony_core_sup:start_link(),
    case Port of
        undefined ->
            ok;
        P when is_integer(P) ->
            ranch:start_listener(pony_tcp, 10,
                                 ranch_tcp, [{port, P}, inet,
                                             raw, binary, {active, false}],
                                 pony_client, [])
    end,
    case SSLPort of
        undefined -> ok;
        SP when is_integer(SP) ->
            {ok, SKey} = application:get_env(pony_core, ssl_key),
            {ok, Cert} = application:get_env(pony_core, ssl_cert),
            ranch:start_listener(pony_ssl, 10,
                                 ranch_ssl, [{port, SP}, inet,
                                             {keyfile, SKey},
                                             {certfile, Cert},
                                             raw, binary, {active, false}],
                                 pony_client, [])
    end,
    {ok, Pid}.

stop(_State) ->
    ranch:stop_listener(pony_tcp),
    ranch:stop_listener(pony_ssl),
    ok.
