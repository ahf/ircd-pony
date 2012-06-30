-module(hostmask).
-export([parse/1]).

parse_nickname_hostname([NicknameAndUsername, Hostname]) ->
    case string:tokens(NicknameAndUsername, "!") of
        [Nickname, Username] ->
            {ok, Nickname, Username, Hostname};
        _ ->
            {error, {invalid_hostmask, NicknameAndUsername}}
    end;

parse_nickname_hostname(X) ->
    {error, {invalid_hostmask, X}}.

parse(String) ->
    parse_nickname_hostname(string:tokens(String, "@")).

