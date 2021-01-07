%%%-------------------------------------------------------------------
%% @doc erl_mysql_oauth public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_mysql_oauth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_mysql_db:start(),
    erl_mysql_oauth_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
