%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc Manage channels in Pony
%%% @end
%%% Created :  5 Jul 2012 by Jesper Louis Andersen <>
%%%-------------------------------------------------------------------
-module(pony_chan_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,
         join/1,
         part/1,
         quits/2
        ]).

-export([channel_members/1,
         part_channel/1,
         join_channel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(TAB, pony_channel_map).
-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(ChanName) ->
    gen_server:call(?SERVER, {join, ChanName}).

part(ChanName) ->
    gen_server:call(?SERVER, {part, ChanName}).

quits(Pid, Name) ->
    gen_server:cast(?SERVER, {quits, Pid, Name}).

channel_members(Channel) ->
    [Pid || {Pid, _} <- gproc:lookup_local_properties({channel, Channel})].

part_channel(Channel) ->
    gproc:unreg({p, l, {channel, Channel}}).

join_channel(Channel) ->
    gproc:add_local_property({channel, Channel}, client).

%%%===================================================================

%% @private
init([]) ->
    ets:new(?TAB, [named_table, protected, bag]),
    {ok, #state{}}.

%% @private
handle_call({join, ChanName}, {Pid, _Tag}, State) ->
    ets:insert(?TAB, {Pid, ChanName}),
    {reply, ok, State};
handle_call({part, ChanName}, {Pid, _Tag}, State) ->
    ets:delete(?TAB, {Pid, ChanName}),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:warning("Unknown Request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({quits, Nick, Pid}, State) when is_pid(Pid) ->
    %% @todo Fix this hack later
    [pony_client:msg(C, {part, Nick, Channel})
     || {_, Channel} <- ets:lookup(?TAB, Pid),
        C <- channel_members(Channel)],
    %% Channels = [Chan || {_, Chan} <- ets:lookup(?TAB, Pid)],
    %% Clients = lists:usort(
    %%             lists:flatmap(fun channel_members/1, Channels)),
    %% [pony_client:msg(C, {quit, Nick}) || C <- Clients],
    {noreply, State};
handle_cast(Msg, State) ->
    lager:warning("Unknown Msg: ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
