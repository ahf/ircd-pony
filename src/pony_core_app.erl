-module(pony_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pony_core_sup:start_link(),
    ranch:start_listener(pony, 100,
                         ranch_tcp, [{port, 6667}],
                         pony_client, []).

stop(_State) ->
    ok.
