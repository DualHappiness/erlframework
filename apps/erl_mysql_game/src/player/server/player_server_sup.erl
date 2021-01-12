%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([new_player/1]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

-spec new_player(PlayerID :: integer()) -> {ok, pid()} | {error, Reason :: term()}.
new_player(PlayerID) ->
    ChildSpec = #{
        id => {player_server, PlayerID},
        start => {player_server, start_link, []}
    },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _Info} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.
