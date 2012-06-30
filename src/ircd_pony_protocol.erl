-module(ircd_pony_protocol).
-export([parse/1]).

parse_argument(<<$:, X/binary>>) ->
    [X];
parse_argument(<<X/binary>>) ->
    case binary:split(X, <<" ">>) of
        [Head, Tail] -> [Head | parse_argument(Tail)];
        _ -> [X]
    end.

parse_command(Prefix, X) ->
    case binary:split(X, <<" ">>) of
        [Command, Rest] -> {ok, Prefix, Command, parse_argument(Rest)};
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
