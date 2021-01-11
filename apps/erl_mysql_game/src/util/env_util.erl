-module(env_util).

-export([get_config_path/0, get_config_strategy/0]).
-export([get_server_ids/0]).
-export([get_player_id_start/0]).

-spec get_config_path() -> string().
get_config_path() ->
    {ok, Path} = application:get_env(
        erl_mysql_game,
        config_path,
        "./config/game_config.config"
    ),
    Path.

-spec get_server_ids() -> [non_neg_integer()].
get_server_ids() ->
    {ok, ServerID} = application:get_env(erl_mysql_game, server_id, [0]),
    ServerID.

-spec get_config_strategy() -> normal | force_init.
get_config_strategy() ->
    {ok, Strategy} = application:get_env(erl_mysql_game, config_strategy, normal),
    Strategy.

-spec get_player_id_start() -> non_neg_integer().
get_player_id_start() ->
    {ok, V} = application:get_env(erl_mysql_game, player_id_start, 0),
    V.
