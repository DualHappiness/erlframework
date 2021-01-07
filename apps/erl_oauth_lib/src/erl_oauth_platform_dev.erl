-module(erl_oauth_platform_dev).

-behaviour(erl_oauth_platform).

-include("erl_oauth_lib.hrl").

-export([auth/1]).

-spec auth(Req :: #auth_body{platform :: dev}) -> erl_oauth_platform:auth_ret().
auth(#auth_body{platform = dev, openid = ID}) ->
    {ok, #auth_ret{platform = dev, accname = ID}}.

