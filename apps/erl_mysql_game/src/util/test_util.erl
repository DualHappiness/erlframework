-module(test_util).

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

-define(GET_MSG,
    receive
        {_, {_, Msg}} -> Msg
    end
).

-import(player_server_mgr, [route_msg/2]).

all() ->
    [common_test, gen_mod_test, close_test, relogin_test].

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

    {ok, ok} = route_msg({Auth, test_msg(), Self}, Args),
    ?MATCH_MSG(#system_error_s2c{code = ?E_PLAYER_NEED_ENTER}),

    {ok, ok} = route_msg({Auth, enter_msg(), Self}, Args),
    ?MATCH_MSG(#acc_enter_s2c{}),
    ok.

gen_mod_test(_Config) ->
    ok.

make_client() ->
    Auth = auth_ret(),
    Args = [],
    Parent = self(),
    Client = spawn(fun() ->
        process_flag(trap_exit, true),
        {ok, ok} = route_msg({Auth, enter_msg(), self()}, Args),
        #acc_enter_s2c{} = ?GET_MSG,
        Parent ! enter_done,
        receive
            close ->
                close;
            Other ->
                Parent ! Other
        end
    end),
    receive
        enter_done -> ok
    end,
    [{_, ID, {_, Pid}}] = ets:lookup(
        player_server_mgr,
        {Auth#auth_ret.accname, Auth#auth_ret.platform}
    ),
    ?assertNotMatch(undefined, process_info(Pid)),
    ?assertMatch(Pid, player_server_mgr:get_pid(ID)),
    {Client, ID, Pid}.

close_test(_Config) ->
    ?LET(
        _,
        ok,
        begin
            {Client, ID, Pid} = make_client(),
            Client ! close,
            receive
            after 1000 -> ok
            end,

            Auth = auth_ret(),
            ?assertMatch(
                [],
                ets:lookup(player_server_mgr, {Auth#auth_ret.accname, Auth#auth_ret.platform})
            ),
            ?assertMatch(undefined, process_info(Pid)),
            ?assertMatch(undefined, player_server_mgr:get_pid(ID))
        end
    ),

    ?LET(_, ok, begin
        {Client, ID, Pid} = make_client(),
        player_server:stop(Pid, ?E_SYSTEM),
        receive
            {'EXIT', Pid, {_, {_, #system_error_s2c{code = ?E_SYSTEM}}}} -> ok
        after 1000 -> throw(stop_error)
        end,
        ?assertMatch(undefined, process_info(Client)),
        ?assertMatch(undefined, process_info(Pid)),
        ?assertMatch(undefined, player_server_mgr:get_pid(ID))
    end),
    ok.

relogin_test(_Config) ->
    {Client1, ID, Pid1} = make_client(),
    {Client2, ID, Pid2} = make_client(),
    receive
        {'EXIT', Pid1, {_, {_, #acc_relogin_s2c{}}}} -> ok
    after 1000 -> throw(relogin_error)
    end,

    ?assertMatch(undefined, process_info(Client1)),
    ?assertNotMatch(undefined, process_info(Client2)),
    ?assertMatch(undefined, process_info(Pid1)),
    ?assertMatch(Pid2, player_server_mgr:get_pid(ID)),
    ok.
