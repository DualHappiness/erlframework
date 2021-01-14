-module(player_id_server_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-incldue_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(APPNAME, erl_mysql_game).
-define(ENV_OPT, [{timeout, infinity}, {persistent, true}]).

all() ->
    [common_test, thread_test].

init_per_suite(_Config) ->
    ConfigPath = filename:join(code:priv_dir(?APPNAME), "test_config"),
    application:set_env(?APPNAME, config_path, ConfigPath, ?ENV_OPT),
    % ok = application:get_env(?APPNAME, config_path),
    {ok, _} = application:ensure_all_started(?APPNAME),
    [].

end_per_suite(_Config) ->
    ok = application:stop(?APPNAME).

common_test(_Config) ->
    application:set_env(?APPNAME, player_id_start, 100, ?ENV_OPT),
    application:set_env(?APPNAME, server_ids, [10, 1, 100], ?ENV_OPT),
    application:set_env(?APPNAME, config_strategy, force_init, ?ENV_OPT),
    ok = application:stop(?APPNAME),
    {ok, _} = application:ensure_all_started(?APPNAME),

    ID0 = player_id_server:get_new(),
    ?assert(
        ID0 >
            1 *
                100_000_000 + 100
    ),
    lists:foldl(
        fun(_, Last) ->
            ID = player_id_server:get_new(),
            ?assertEqual(Last + 1, ID),
            ID
        end,
        ID0,
        lists:seq(1, 10000)
    ),

    ok.

thread_test(_Config) ->
    Parent = self(),
    lists:foreach(
        fun(I) ->
            spawn(fun() ->
                Parent ! {I, player_id_server:get_new()}
            end)
        end,
        lists:seq(1, 10000)
    ),

    M = maps:from_list([
        receive
            {I, ID} -> {ID, I}
        end
        || _ <- lists:seq(1, 10000)
    ]),
    ?assertEqual(10000, maps:size(M)),
    ok.
