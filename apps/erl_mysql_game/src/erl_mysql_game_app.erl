%%%-------------------------------------------------------------------
%% @doc erl_mysql_game public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_mysql_game_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_mysql_game_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
