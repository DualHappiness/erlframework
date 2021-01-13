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

-export([new/1]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [#{start => {player_data, start_link, []}, restart => temporary}],
    {ok, {SupFlags, ChildSpecs}}.

-spec new(ID :: term()) -> {ok, pid()} | {error, Reason :: term()}.
new(ID) ->
    case supervisor:start_child(?MODULE, [ID]) of
        {ok, ChildPid} ->
            {ok, ChildPid};
        {ok, ChildPid, _Info} ->
            {ok, ChildPid};
        {error, Reason} ->
            {error, Reason}
    end.
