-module(ircd_pony_client).

-export([start_link/4]).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { transport, socket, listener, synchronized }).

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
    %% This should be called when we know the handshake went well
%%    ranch:accept_ack(ListenerPid),
    %% Quickly Close the socket
    %%    Transport:close(Socket),
    Transport:setopts(Socket,
                      [raw, binary]),
    {ok, #state{ listener = ListenerPid,
                 transport = Transport,
                 socket = Socket,
                 synchronized = false }, 0}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state { synchronized = false,
                              transport = Transport,
                              socket = Socket } = State) ->
    sync(Socket),
    Transport:setopts(Socket, {active, once}),
    {noreply, State};
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

%% @doc Synchronize the socket
sync(_Socket) ->
    todo.

