-module(erl_oauth_lib).

-include("erl_oauth_lib.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([auth/1]).

-spec auth(Req :: #auth_body{}) -> erl_oauth_platform:auth_ret().
auth(Req) ->
    Mod = get_handler(Req),
    Mod:auth(Req).

get_handler(_) ->
    erl_oath_platform_error.