%%%-------------------------------------------------------------------
%% @doc erl_mysql_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_mysql_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_mysql_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
