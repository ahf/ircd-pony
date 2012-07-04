-module(pony_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = pony_core_sup:start_link(),
    ranch:start_listener(pony, 10,
                         ranch_tcp, [{port, 6667}, raw, binary, {active, false}],
                         pony_client, []),
    {ok, Pid}.

stop(_State) ->
    ranch:stop_listener(pony),
    ok.
