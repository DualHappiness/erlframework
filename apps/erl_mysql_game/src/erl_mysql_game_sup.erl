%%%-------------------------------------------------------------------
%% @doc erl_mysql_game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_mysql_game_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{
            id => player_sup,
            start => {player_sup, start_link, []},
            type => supervisor,
            modules => [player_sup]
        },
        #{
            id => servers_sup,
            start => {servers_sup, start_link, []},
            type => supervisor,
            modules => [servers_sup]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
