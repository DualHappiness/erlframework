-module(erl_oauth_platfrom_error).

-behaviour(erl_oauth_platform).

-export([auth/1]).

-spec auth(any()) -> {error, unsupport}.
auth(_) ->
    {error, unsupport}.