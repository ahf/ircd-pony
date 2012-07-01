-module(pony_client).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { socket, listener,
                 synchronized = no :: 'no' | {'yes', string()}
               }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenerPid,
                                                      Socket,
                                                      Transport,
                                                      Opts], []).    

%%%===================================================================

%% @private
init([ListenerPid, Socket, Transport, _Opts]) ->
    Transport:setopts(Socket, [raw, binary]),
    {ok, #state{ listener = ListenerPid,
                 socket = {Transport, Socket},
                 synchronized = no }, 0}. %% Note immediate timeout

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state { synchronized = no,
                              listener = Listener,
                              socket = Socket } = State) ->
    {ok, Hostname} = sync(Socket),
    %% We only let ranch continue when this client has been accepted
    %% This effectively throttles the inbound connection so we at most
    %% process a limited amount of new connections
    ranch:accept_ack(Listener),
    ack(Socket),
    {noreply, State#state { synchronized = {yes, Hostname} }};
handle_info(Info, State) ->
    lager:warning("Unknown message received: ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

ack({Transport, Socket}) ->
    Transport:setopts(Socket, [{active, once}]).

%% @doc Synchronize the socket
sync(Sock) ->
    out(Sock, "NOTICE AUTH :*** Processing connection to ~s ...",
        [pony:me()]),
    out(Sock, "NOTICE AUTH :*** Looking up your hostname ..."),
    {ok, Hostname} = lookup_hostname(Sock),
    out(Sock, "NOTICE AUTH :*** Found your hostname (~s) ...", [Hostname]),
    %% @todo Unregistered proto handling goes here
    {ok, Hostname}.

out({Transport, Socket}, Data) ->
    Transport:send(Socket, [Data, <<"\r\n">>]).

out({Transport, Socket}, Format, Params) ->
    Transport:send(Socket, [io_lib:format(Format, Params), <<"\r\n">>]).

lookup_hostname({_Transport, Socket}) ->
    %% @todo should probably be a service on its own
    {ok, {Address, _Port}} = inet:peername(Socket),
    {ok, #hostent { h_name = Hostname }} = inet:gethostbyaddr(Address),
    {ok, Hostname}.
