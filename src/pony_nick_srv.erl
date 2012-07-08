%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc A Nick Registry handler
%%%
%%% @end
%%% Created :  4 Jul 2012 by Jesper Louis Andersen <>
%%%-------------------------------------------------------------------
-module(pony_nick_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, swap/2, unregister/1]).
-export([lookup_nick/1, nick/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



-define(TAB, pony_nick_registry).
-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

swap(N, N) -> ok; % No-op swap
swap(<<"*">>, Name) ->
    case ets:lookup(?TAB, Name) of
        [] ->
            gen_server:call(?SERVER, {register, Name});
        [_|_] ->
            nick_in_use
    end.

unregister(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

nick(Pid) when is_pid(Pid) ->
    case ets:lookup(?TAB, Pid) of
        [] ->
            none;
        [{_, N}] -> {value, N}
    end.

lookup_nick(Recipient) ->
    gproc:lookup_local_name({nick, Recipient}).

%%%===================================================================


%% @private
init([]) ->
    ets:new(?TAB, [named_table, protected]),
    {ok, #state{}}.

%% @private
handle_call({register, Name}, {Pid, _Tag}, State) ->
    case ets:lookup(?TAB, Name) of
        [] ->
            _Ref = erlang:monitor(process, Pid),
            ets:insert(?TAB, {Name, Pid}),
            ets:insert(?TAB, {Pid, Name}),
            {reply, ok, State};
        [_|_] ->
            {reply, nick_in_use, State}
    end;
handle_call({unregister, Name}, _From, State) ->
    case ets:lookup(?TAB, Name) of
        [{Name, MonitorRef}] ->
            erlang:demonitor(MonitorRef, [flush]),
            ets:delete(?TAB, Name),
            {reply, ok, State};
        [] ->
            {reply, {error, {no_such_nick, Name}}, State}
    end;
handle_call(Request, _From, State) ->
    lager:warning("Unknown call request: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    [{Pid, Name}] = ets:lookup(?TAB, Pid),
    ets:delete(?TAB, Name),
    ets:delete(?TAB, Pid),
    pony_chan_srv:quits(Pid, Name),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

