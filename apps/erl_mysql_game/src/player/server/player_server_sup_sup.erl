%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_server_sup_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("datatable/include/datatable.hrl").

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Tab = ets:new(player_server_mgr, [public, set, named_table, {read_concurrency, true}]),
    [
        begin
            datatable:start([#datatable_declare{table = Table}]),
            erl_mysql_datatable_loader:gen_loader(Table)
        end
        || Table <- [account2player, player]
    ],

    SupFlags = #{strategy => one_for_all, intensity => 10, period => 5},
    ChildSpecs = [
        #{
            id => player_server_mgr,
            start => {player_server_mgr, start_link, [Tab]},
            shutdown => 50000,
            modules => [player_server_mgr]
        },
        #{
            id => player_server_sup,
            start => {player_server_sup, start_link, []},
            type => supervisor,
            modules => [player_server_sup]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
