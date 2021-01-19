-module(env_util).

-export([get_config_path/0, get_config_strategy/0]).
-export([get_server_ids/0]).
-export([get_player_id_start/0]).

-spec get_config_path() -> string().
get_config_path() ->
    application:get_env(
        erl_mysql_game,
        config_path,
        "config/game_config.dets"
    ).

-spec get_server_ids() -> [non_neg_integer()].
get_server_ids() ->
    application:get_env(erl_mysql_game, server_ids, [0]).

-spec get_config_strategy() -> normal | force_init.
get_config_strategy() ->
    application:get_env(erl_mysql_game, config_strategy, normal).

-spec get_player_id_start() -> non_neg_integer().
get_player_id_start() ->
    application:get_env(erl_mysql_game, player_id_start, 0).
