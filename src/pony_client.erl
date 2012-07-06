-module(pony_client).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").

%% API
-export([start_link/4, msg/2, numeric/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { socket, listener,
                 cont = <<>> :: binary(),
                 synchronized = no :: 'no' | 'yes',
                 username = <<"*">>,
                 realname = <<"*">>,
                 nickname = <<"*">>,
                 hostname = undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the server
%% @end
start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid,
                                    Socket,
                                    Transport,
                                    Opts], []).    

msg(Client, Msg) ->
    gen_server:cast(Client, {msg, Msg}).

numeric(Client, Numeric, Args) ->
    gen_server:cast(Client, {numeric, Numeric, Args}).

%%%===================================================================

%% @private
init([ListenerPid, Socket, Transport, _Opts]) ->
    {ok, #state{ listener = ListenerPid,
                 socket = {Transport, Socket},
                 cont = <<>>,
                 synchronized = no }, 0}. %% Note immediate timeout

%% @private
handle_call(Request, _From, State) ->
    lager:debug("Unknown request in call: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({numeric, Numeric, Args},
            #state { socket = Sock } = State) ->
    out(Sock, pony_protocol:render_numeric(Numeric, Args)),
    {noreply, State};
handle_cast({msg, {privmsg, Nick, _Target, _Text}},
            #state { nickname = Nick } = State) ->
    %% Quaff messages to self
    {noreply, State};
handle_cast({msg, M}, #state { socket = Sock } = State) ->
    out(Sock, pony_protocol:render(M)),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:debug("Unknown message in cast: ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info({tcp_closed, S},
            #state { socket = {_, S} } = State) ->
    {stop, normal, State};
handle_info({ssl_closed, S},
            #state { socket = {_, S} } = State) ->
    {stop, normal, State};
handle_info({tcp, S, Chunk},
            #state { socket = {_, S}} = State) ->
    handle_transport_packet(Chunk, State);
handle_info({ssl, S, Chunk},
            #state { socket = {_, S}} = State) ->
    handle_transport_packet(Chunk, State);
handle_info(timeout, #state { synchronized = no,
                              listener = Listener,
                              socket = Socket } = State) ->
    ranch:accept_ack(Listener),
    {ok, Hostname} = sync(Socket),
    ack_socket(Socket),
    {noreply, State#state { hostname = Hostname } };
handle_info(Info, State) ->
    lager:warning("Unknown message received: ~p State: ~p", [Info, State]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
handle_transport_packet(Chunk, #state { cont = Cont,
                                        socket = Socket } = State) ->
    ack_socket(Socket),
    case process_stream_chunk(Chunk, Cont) of
        {ok, NewCont} ->
            {noreply, State#state { cont = NewCont }};
        {msgs, Msgs, NewCont} ->
            {noreply,
             lists:foldl(fun handle_message/2,
                         State#state { cont = NewCont },
                         Msgs)}
    end.


ack_socket({Transport, Socket}) ->
    Transport:setopts(Socket, [{active, once}]).

send_numeric(Numeric, Args) ->
    numeric(self(), Numeric, Args).

respond(M) ->
    msg(self(), M).

handle_message(M, State) ->
    case pony_protocol:parse(M) of
        {ok, Prefix, Command, Args} ->
            handle_message(Prefix, Command, Args, State);
        {error, _} ->
            lager:warning("Protocol parse error: ~p", [M]),
            %% Ignore errors for now
            State
    end.


handle_message(Prefix, Command, Args, #state { synchronized = no } = State) ->
    case handle_nick_user(Prefix, Command, Args, State) of
        #state { nickname = Nick, username = User} = NewState
          when Nick /= <<"*">>, User /= <<"*">> ->
            send_numeric('RPL_WELCOME', [Nick]),
            send_numeric('RPL_YOURHOST', [Nick]),
            %% this.SendMotd()
            NewState#state { synchronized = yes };
        NewState ->
            NewState
    end;
handle_message(Prefix, Command, Args, #state { nickname = CurNick } = State) ->
    lager:debug("Got Incoming Message: ~p", [[{prefix, Prefix},
                                              {command, Command},
                                              {args, Args}]]),
    case {Prefix, Command, Args} of
        {<<>>, ping, []} ->
            send_numeric('ERR_NEEDMOREPARAMS', [CurNick, ping]),
            State;
        {<<>>, ping, [Server]} ->
            respond({pong, Server}),
            State;
        {<<>>, privmsg, []} ->
            send_numeric('ERR_NORECIPIENT', [CurNick, privmsg]),
            State;
        {<<>>, privmsg, [_Recipient]} ->
            send_numeric('ERR_NOTEXTTOSEND', [CurNick]),
            State;
        {<<>>, privmsg, [Recipient, Text]} ->
            PM = {privmsg, CurNick, Recipient, Text},
            case pony_nick_srv:lookup_nick(Recipient) of
                undefined ->
                    case pony_chan_srv:channel_members(Recipient) of
                        [] ->
                            State;
                        Members when is_list(Members) ->
                            [pony_client:msg(Pid, PM) || Pid <- Members]
                    end,
                    State;
                Pid when is_pid(Pid) ->
                    pony_client:msg(Pid, PM),
                    State
            end;
        {<<>>, quit, [_Message]} ->
            State;
        {<<>>, topic, []} ->
            send_numeric('ERR_NEEDMOREPARAMS', [CurNick, topic]),
            State;
        {<<>>, topic, [Channel]} ->
            case pony_chan_srv:lookup_topic(Channel) of
                {topic, T} ->
                    send_numeric('RPL_TOPIC', [CurNick, Channel, T]);
                no_topic ->
                    send_numeric('RPL_NOTOPIC', [CurNick, Channel])
            end,
            State;
        {<<>>, topic, [Channel, Text]} ->
            pony_chan_srv:set_topic(Channel, Text),
            State;
        {<<>>, join, []} ->
            send_numeric('ERR_NEEDMOREPARAMS', [CurNick, join]),
            State;
        {<<>>, join, [Channel]} ->
            ok = pony_chan_srv:join(Channel),
            pony_chan_srv:join_channel(Channel),
            [msg(Pid, {join, CurNick, Channel})
             || Pid <- pony_chan_srv:channel_members(Channel)],
            State;
        {<<>>, part, []} ->
            send_numeric('ERR_NEEDMOREPARAMS', [CurNick, part]),
            State;
        {<<>>, part, [Channel]} ->
            ok = pony_chan_srv:part(Channel),
            [msg(Pid, {part, CurNick, Channel})
             || Pid <- pony_chan_srv:channel_members(Channel)],
            pony_chan_srv:part_channel(Channel),
            State;
        _ ->
            handle_nick_user(Prefix, Command, Args, State)
    end.

handle_nick_user(Prefix, Command, Args, #state { nickname = CurNick } = State) ->
    case {Prefix, Command, Args} of
        {<<>>, nick, []} ->
            send_numeric('ERR_NONICKNAMEGIVEN', [CurNick]),
            State;
        {<<>>, nick, [NickName]} ->
            case pony_nick_srv:swap(CurNick, NickName) of
                ok ->
                    gproc:add_local_name({nick, NickName}),
                    State#state { nickname = NickName };
                nick_in_use ->
                    send_numeric('ERR_ERRONEUSNICKNAME', [<<"*">>, NickName]),
                    State
            end;
        {<<>>, user, L}
          when is_list(L), length(L) < 4 ->
            send_numeric('ERR_NEEDMOREPARAMS', [CurNick, user]),
            State;
        {<<>>, user, [Username, _, _, RealName]} ->
            State#state { username = <<"~", Username/binary>>,
                          realname = RealName };
        {Prefix, Command, Args} ->
            lager:debug("Unhandled message: ~p", [[{prefix, Prefix},
                                                   {command, Command},
                                                   {args, Args}]]),
            State
    end.

%% @doc Synchronize the socket
sync(Sock) ->
    out(Sock, "NOTICE AUTH :*** Processing connection to ~s ...",
        [pony:me()]),
    out(Sock, "NOTICE AUTH :*** Looking up your hostname ..."),
    {ok, Hostname} = lookup_hostname(Sock),
    out(Sock, "NOTICE AUTH :*** Found your hostname (~s) ...", [Hostname]),
    {ok, Hostname}.

out({Transport, Socket}, Data) ->
    %% lager:debug("Sending: ~ts", [Data]),
    Transport:send(Socket, [Data, <<"\r\n">>]).

out({Transport, Socket}, Format, Params) ->
    Transport:send(Socket, [io_lib:format(Format, Params), <<"\r\n">>]).

lookup_hostname({Transport, Socket}) ->
    %% @todo should probably be a service on its own
    {ok, {Address, _Port}} = Transport:peername(Socket),
    {ok, #hostent { h_name = Hostname }} = inet:gethostbyaddr(Address),
    {ok, Hostname}.

process_stream_chunk(Chunk, Cont) ->
    process_stream_chunk(Chunk, Cont, []).

process_stream_chunk(Chunk, Cont, Msgs) ->
    Data = <<Cont/binary, Chunk/binary>>,
    case binary:split(Data, <<"\r\n">>, []) of
        [_] ->
            return_messages(Msgs, Data);
        [Line, Rest] ->
            process_stream_chunk(<<>>, Rest, [Line | Msgs])
    end.

return_messages([], Data) -> {ok, Data};
return_messages(Msgs, Data) -> {msgs, lists:reverse(Msgs), Data}.
