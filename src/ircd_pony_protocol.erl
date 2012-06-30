-module(ircd_pony_protocol).
-export([parse/1]).

command_to_atom(X) when is_binary(X) ->
    command_to_atom(binary_to_list(X));
command_to_atom(X) when is_list(X) ->
    case string:to_lower(X) of
        "admin" -> admin;
        "away" -> away;
        "invite" -> invite;
        "join" -> join;
        "kick" -> kick;
        "kill" -> kill;
        "list" -> list;
        "mode" -> mode;
        "motd" -> motd;
        "names" -> names;
        "nick" -> nick;
        "notice" -> notice;
        "oper" -> oper;
        "part" -> part;
        "pass" -> pass;
        "ping" -> ping;
        "privmsg" -> privmsg;
        "quit" -> quit;
        "topic" -> topic;
        "user" -> user;
        "version" -> version;
        "who" -> who;
        "whois" -> whois;
        "whowas" -> whowas;
        _ -> {unknown, X}
    end.

parse_argument(<<$:, X/binary>>) ->
    [X];
parse_argument(<<X/binary>>) ->
    case binary:split(X, <<" ">>) of
        [Head, Tail] -> [Head | parse_argument(Tail)];
        _ -> [X]
    end.

parse_command(Prefix, X) ->
    case binary:split(X, <<" ">>) of
        [Command, Rest] -> {ok, Prefix, command_to_atom(Command), parse_argument(Rest)};
        _ -> {error, {missing_command, Prefix, X}}
    end.

parse_prefix(<<$:, X/binary>>) ->
    case binary:split(X, <<" ">>) of
        [Prefix, Rest] -> parse_command(Prefix, Rest);
        _ -> {error, {missing_command, X}}
    end;
parse_prefix(<<Rest/binary>>) ->
    parse_command(<<>>, Rest).

parse(X) when is_binary(X) ->
    parse_prefix(X);
parse(X) when is_list(X) ->
    parse(list_to_binary(X)).
