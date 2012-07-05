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
         quits/1]).

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

quits(Pid) ->
    gen_server:cast(?SERVER, {quits, Pid}).

%%%===================================================================

%% @private
init([]) ->
    ets:new(?TAB, [named_table, protected, bag]),
    {ok, #state{}}.

%% @private
handle_call({join, ChanName}, {Pid, _Tag}, State) ->
    ets:insert(?TAB, {Pid, ChanName}),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:warning("Unknown Request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({quits, Pid}, State) when is_pid(Pid) ->
    %% @todo Handle quits
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


