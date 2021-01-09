%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% 
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data_sup_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} |
                      {error, {already_started, pid()}} |
                      {error, Reason :: term()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.