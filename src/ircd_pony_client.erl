-module(ircd_pony_client).

-export([start_link/4]).

start_link(ListenerPid, Socket, Transport, _Opts) ->
    ranch:accept_ack(ListenerPid),
    Transport:close(Socket),
    ok.


