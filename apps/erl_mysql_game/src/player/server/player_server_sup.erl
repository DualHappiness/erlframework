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

-export([new_player/2]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 5},
    ChildSpecs = [
        #{
            start => {player_server, start_link, []},
            restart => temporary
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

-spec new_player(PlayerID :: integer(), Client :: pid()) -> {ok, pid()} | {error, Reason :: term()}.
new_player(PlayerID, Client) ->
    case supervisor:start_child(?MODULE, [PlayerID, Client]) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _Info} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

% -spec get_children() -> 
