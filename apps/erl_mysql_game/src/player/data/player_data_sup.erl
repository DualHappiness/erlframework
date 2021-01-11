%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% description
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([new_player_data/1]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

-spec new_player_data(ID :: term()) -> {ok, pid()} | {error, Reason :: term()}.
new_player_data(ID) ->
    Child = #{id => {player_data, ID}, start => {player_data, start_link, [ID]}},
    case supervisor:start_child(?MODULE, Child) of
        {ok, ChildPid} ->
            {ok, ChildPid};
        {ok, ChildPid, _Info} ->
            {ok, ChildPid};
        {error, Reason} ->
            {error, Reason}
    end.
