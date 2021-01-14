-module(player_server_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-incldue_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("erl_oauth_lib/include/erl_oauth_lib.hrl").
-include_lib("proto_container/include/demo_proto_error_code.hrl").
-include_lib("proto_container/include/demo_proto_acc.hrl").
-include_lib("proto_container/include/demo_proto_system.hrl").
-include_lib("proto_container/include/demo_proto_test.hrl").

-define(APPNAME, erl_mysql_game).
-define(ENV_OPT, [{timeout, infinity}, {persistent, true}]).
-define(MATCH_MSG(Guard),
    (fun() ->
        ?assertMatch(
            Guard,
            receive
                {_, {_, LocalMsg}} -> LocalMsg
            after 1000 -> throw({no_msg, ?LINE})
            end
        )
    end)()
).

-import(player_server_mgr, [route_msg/2]).

all() ->
    [common_test].

init_per_suite(_Config) ->
    %% TODO 自动清理数据库
    ConfigPath = filename:join(code:priv_dir(?APPNAME), "test_config"),
    application:set_env(?APPNAME, config_path, ConfigPath, ?ENV_OPT),
    % ok = application:get_env(?APPNAME, config_path),
    {ok, _} = application:ensure_all_started(?APPNAME),
    [].

end_per_suite(_Config) ->
    ok = application:stop(?APPNAME).

auth_ret() ->
    #auth_ret{accname = <<"ct_123">>, platform = dev}.

test_msg() ->
    Mod = {test, part1},
    Rec = #test_part1_c2s{value = 1},
    {Mod, Rec}.

create_msg() ->
    Mod = {acc, create},
    Rec = #acc_create_c2s{},
    {Mod, Rec}.

enter_msg() ->
    Mod = {acc, enter},
    Rec = #acc_enter_c2s{id = 1},
    {Mod, Rec}.

msg_maker(Auth, Msg) ->
    {Auth, Msg, self()}.

common_test(_Config) ->
    Auth = auth_ret(),
    Self = self(),
    Args = [],

    {ok, ok} = route_msg({Auth, enter_msg(), Self}, Args),
    ?MATCH_MSG(#system_error_s2c{code = ?E_PLAYER_NO_ROLE}),

    {ok, ok} = route_msg({Auth, create_msg(), Self}, [{ip, {127, 0, 0, 1}}]),
    ?MATCH_MSG(#acc_create_s2c{}),

    {ok, ok} = route_msg({Auth, create_msg(), Self}, [{ip, {127, 0, 0, 1}}]),
    ?MATCH_MSG(#system_error_s2c{code = ?E_PLAYER_ALREADY_CREATED}),

    {ok, ok} =route_msg({Auth, test_msg(), Self}, Args),
    ?MATCH_MSG(#system_error_s2c{code = ?E_PLAYER_NEED_ENTER}),

    {ok, ok} = route_msg({Auth, enter_msg(), Self}, Args),
    ?MATCH_MSG(#acc_enter_s2c{}),
    ok.

gen_mod_test() ->
    ok.

close_test() ->
    ok.