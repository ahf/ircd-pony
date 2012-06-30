-module(ircd_pony_protocol).
-export([parse/1]).
-export([numeric/1]).

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

numeric(N) when is_atom(N) ->
    case N of
        'RPL_WELCOME'           -> 0;
        'RPL_YOURHOST'          -> 2;
        'RPL_CREATED'           -> 3;
        'RPL_MYINFO'            -> 4;
        'RPL_ISUPPORT'          -> 5;
        'RPL_REDIR'             -> 10;
        'RPL_MAP'               -> 15;
        'RPL_MAPMORE'           -> 16;
        'RPL_MAPEND'            -> 17;
        'RPL_SAVENICK'          -> 43;
        'RPL_TRACELINK'         -> 200;
        'RPL_TRACECONNECTING'   -> 201;
        'RPL_TRACEHANDSHAKE'    -> 202;
        'RPL_TRACEUNKNOWN'      -> 203;
        'RPL_TRACEOPERATOR'     -> 204;
        'RPL_TRACEUSER'         -> 205;
        'RPL_TRACESERVER'       -> 206;
        'RPL_TRACENEWTYPE'      -> 208;
        'RPL_TRACECLASS'        -> 209;
        'RPL_STATSLINKINFO'     -> 211;
        'RPL_STATSCOMMANDS'     -> 212;
        'RPL_STATSCLINE'        -> 213;
        'RPL_STATSNLINE'        -> 214;
        'RPL_STATSILINE'        -> 215;
        'RPL_STATSKLINE'        -> 216;
        'RPL_STATSQLINE'        -> 217;
        'RPL_STATSYLINE'        -> 218;
        'RPL_ENDOFSTATS'        -> 219;
        'RPL_STATSPLINE'        -> 220;
        'RPL_UMODEIS'           -> 221;
        'RPL_STATSFLINE'        -> 224;
        'RPL_STATSDLINE'        -> 225;
        'RPL_SERVLIST'          -> 234;
        'RPL_SERVLISTEND'       -> 235;
        'RPL_STATSLLINE'        -> 241;
        'RPL_STATSUPTIME'       -> 242;
        'RPL_STATSOLINE'        -> 243;
        'RPL_STATSHLINE'        -> 244;
        'RPL_STATSSLINE'        -> 245;
        'RPL_STATSXLINE'        -> 247;
        'RPL_STATSULINE'        -> 248;
        'RPL_STATSDEBUG'        -> 249;
        'RPL_STATSCONN'         -> 250;
        'RPL_LUSERCLIENT'       -> 251;
        'RPL_LUSEROP'           -> 252;
        'RPL_LUSERUNKNOWN'      -> 253;
        'RPL_LUSERCHANNELS'     -> 254;
        'RPL_LUSERME'           -> 255;
        'RPL_ADMINME'           -> 256;
        'RPL_ADMINLOC1'         -> 257;
        'RPL_ADMINLOC2'         -> 258;
        'RPL_ADMINEMAIL'        -> 259;
        'RPL_TRACELOG'          -> 261;
        'RPL_ENDOFTRACE'        -> 262;
        'RPL_LOAD2HI'           -> 263;
        'RPL_LOCALUSERS'        -> 265;
        'RPL_GLOBALUSERS'       -> 266;
        'RPL_ACCEPTLIST'        -> 281;
        'RPL_ENDOFACCEPT'       -> 282;
        'RPL_NONE'              -> 300;
        'RPL_AWAY'              -> 301;
        'RPL_USERHOST'          -> 302;
        'RPL_ISON'              -> 303;
        'RPL_TEXT'              -> 304;
        'RPL_UNAWAY'            -> 305;
        'RPL_NOWAWAY'           -> 306;
        'RPL_WHOISUSER'         -> 311;
        'RPL_WHOISSERVER'       -> 312;
        'RPL_WHOISOPERATOR'     -> 313;
        'RPL_WHOWASUSER'        -> 314;
        'RPL_ENDOFWHO'          -> 315;
        'RPL_ENDOFWHOWAS'       -> 369;
        'RPL_WHOISCHANOP'       -> 316;
        'RPL_WHOISIDLE'         -> 317;
        'RPL_ENDOFWHOIS'        -> 318;
        'RPL_WHOISCHANNELS'     -> 319;
        'RPL_LISTSTART'         -> 321;
        'RPL_LIST'              -> 322;
        'RPL_LISTEND'           -> 323;
        'RPL_CHANNELMODEIS'     -> 324;
        'RPL_CREATIONTIME'      -> 329;
        'RPL_WHOISLOGGEDIN'     -> 330;
        'RPL_NOTOPIC'           -> 331;
        'RPL_TOPIC'             -> 332;
        'RPL_TOPICWHOTIME'      -> 333;
        'RPL_WHOISACTUALLY'     -> 338;
        'RPL_INVITING'          -> 341;
        'RPL_SUMMONING'         -> 342;
        'RPL_INVITELIST'        -> 346;
        'RPL_ENDOFINVITELIST'   -> 347;
        'RPL_EXCEPTLIST'        -> 348;
        'RPL_ENDOFEXCEPTLIST'   -> 349;
        'RPL_VERSION'           -> 351;
        'RPL_WHOREPLY'          -> 352;
        'RPL_NAMREPLY'          -> 353;
        'RPL_KILLDONE'          -> 361;
        'RPL_CLOSING'           -> 362;
        'RPL_CLOSEEND'          -> 363;
        'RPL_LINKS'             -> 364;
        'RPL_ENDOFLINKS'        -> 365;
        'RPL_ENDOFNAMES'        -> 366;
        'RPL_BANLIST'           -> 367;
        'RPL_ENDOFBANLIST'      -> 368;
        'RPL_INFO'              -> 371;
        'RPL_MOTD'              -> 372;
        'RPL_INFOSTART'         -> 373;
        'RPL_ENDOFINFO'         -> 374;
        'RPL_MOTDSTART'         -> 375;
        'RPL_ENDOFMOTD'         -> 376;
        'RPL_YOUREOPER'         -> 381;
        'RPL_REHASHING'         -> 382;
        'RPL_MYPORTIS'          -> 384;
        'RPL_NOTOPERANYMORE'    -> 385;
        'RPL_RSACHALLENGE'      -> 386;
        'RPL_TIME'              -> 391;
        'RPL_USERSSTART'        -> 392;
        'RPL_USERS'             -> 393;
        'RPL_ENDOFUSERS'        -> 394;
        'RPL_NOUSERS'           -> 395;
        'ERR_NOSUCHNICK'        -> 401;
        'ERR_NOSUCHSERVER'      -> 402;
        'ERR_NOSUCHCHANNEL'     -> 403;
        'ERR_CANNOTSENDTOCHAN'  -> 404;
        'ERR_TOOMANYCHANNELS'   -> 405;
        'ERR_WASNOSUCHNICK'     -> 406;
        'ERR_TOOMANYTARGETS'    -> 407;
        'ERR_NOORIGIN'          -> 409;
        'ERR_INVALIDCAPCMD'     -> 410;
        'ERR_NORECIPIENT'       -> 411;
        'ERR_NOTEXTTOSEND'      -> 412;
        'ERR_NOTOPLEVEL'        -> 413;
        'ERR_WILDTOPLEVEL'      -> 414;
        'ERR_TOOMANYMATCHES'    -> 416;
        'ERR_UNKNOWNCOMMAND'    -> 421;
        'ERR_NOMOTD'            -> 422;
        'ERR_NOADMININFO'       -> 423;
        'ERR_FILEERROR'         -> 424;
        'ERR_NONICKNAMEGIVEN'   -> 431;
        'ERR_ERRONEUSNICKNAME'  -> 432;
        'ERR_NICKNAMEINUSE'     -> 433;
        'ERR_NICKCOLLISION'     -> 436;
        'ERR_UNAVAILRESOURCE'   -> 437;
        'ERR_NICKTOOFAST'       -> 438;
        'ERR_USERNOTINCHANNEL'  -> 441;
        'ERR_NOTONCHANNEL'      -> 442;
        'ERR_USERONCHANNEL'     -> 443;
        'ERR_NOLOGIN'           -> 444;
        'ERR_SUMMONDISABLED'    -> 445;
        'ERR_USERSDISABLED'     -> 446;
        'ERR_NOTREGISTERED'     -> 451;
        'ERR_ACCEPTFULL'        -> 456;
        'ERR_ACCEPTEXIST'       -> 457;
        'ERR_ACCEPTNOT'         -> 458;
        'ERR_NEEDMOREPARAMS'    -> 461;
        'ERR_ALREADYREGISTRED'  -> 462;
        'ERR_NOPERMFORHOST'     -> 463;
        'ERR_PASSWDMISMATCH'    -> 464;
        'ERR_YOUREBANNEDCREEP'  -> 465;
        'ERR_YOUWILLBEBANNED'   -> 466;
        'ERR_KEYSET'            -> 467;
        'ERR_CHANNELISFULL'     -> 471;
        'ERR_UNKNOWNMODE'       -> 472;
        'ERR_INVITEONLYCHAN'    -> 473;
        'ERR_BANNEDFROMCHAN'    -> 474;
        'ERR_BADCHANNELKEY'     -> 475;
        'ERR_BADCHANMASK'       -> 476;
        'ERR_NEEDREGGEDNICK'    -> 477;
        'ERR_BANLISTFULL'       -> 478;
        'ERR_BADCHANNAME'       -> 479;
        'ERR_SSLONLYCHAN'       -> 480;
        'ERR_NOPRIVILEGES'      -> 481;
        'ERR_CHANOPRIVSNEEDED'  -> 482;
        'ERR_CANTKILLSERVER'    -> 483;
        'ERR_ISCHANSERVICE'     -> 484;
        'ERR_BANNEDNICK'        -> 485;
        'ERR_VOICENEEDED'       -> 489;
        'ERR_NOOPERHOST'        -> 491;
        'ERR_UMODEUNKNOWNFLAG'  -> 501;
        'ERR_USERSDONTMATCH'    -> 502;
        'ERR_GHOSTEDCLIENT'     -> 503;
        'ERR_USERNOTONSERV'     -> 504;
        'ERR_WRONGPONG'         -> 513;
        'ERR_HELPNOTFOUND'      -> 524;
        'RPL_WHOISSECURE'       -> 671;
        'RPL_MODLIST'           -> 702;
        'RPL_ENDOFMODLIST'      -> 703;
        'RPL_HELPSTART'         -> 704;
        'RPL_HELPTXT'           -> 705;
        'RPL_ENDOFHELP'         -> 706;
        'ERR_TARGCHANGE'        -> 707;
        'RPL_ETRACEFULL'        -> 708;
        'RPL_ETRACE'            -> 709;
        'RPL_KNOCK'             -> 710;
        'RPL_KNOCKDLVR'         -> 711;
        'ERR_TOOMANYKNOCK'      -> 712;
        'ERR_CHANOPEN'          -> 713;
        'ERR_KNOCKONCHAN'       -> 714;
        'ERR_KNOCKDISABLED'     -> 715;
        'ERR_TARGUMODEG'        -> 716;
        'RPL_TARGNOTIFY'        -> 717;
        'RPL_UMODEGMSG'         -> 718;
        'RPL_OMOTDSTART'        -> 720;
        'RPL_OMOTD'             -> 721;
        'RPL_ENDOFOMOTD'        -> 722;
        'ERR_NOPRIVS'           -> 723;
        'RPL_TESTMASK'          -> 724;
        'RPL_TESTLINE'          -> 725;
        'RPL_NOTESTLINE'        -> 726;
        'RPL_TESTMASKGECOS'     -> 727;
        'RPL_MONONLINE'         -> 730;
        'RPL_MONOFFLINE'        -> 731;
        'RPL_MONLIST'           -> 732;
        'RPL_ENDOFMONLIST'      -> 733;
        'ERR_MONLISTFULL'       -> 734;
        'RPL_RSACHALLENGE2'     -> 740;
        'RPL_ENDOFRSACHALLENGE2'-> 741
    end.
