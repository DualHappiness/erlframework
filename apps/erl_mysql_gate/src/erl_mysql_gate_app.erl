%%%-------------------------------------------------------------------
%% @doc erl_mysql_gate public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_mysql_gate_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_mysql_gate_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
