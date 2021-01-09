-module(erl_mysql_test_util).

-include_lib("proto_container/include/demo_proto_acc.hrl").

-compile(export_all).
-compile(nowarn_export_all).

connect() ->
    {ok, Ls} = application:get_env(erl_gate, listeners),
    #{ip := IP, port := [Port | _]} = hd(Ls),
    {ok, S} = gen_tcp:connect(IP, Port, []),
    S.

login_msg() ->
    #acc_login_c2s{platform = <<"dev">>,
                   game_account_id = <<"123123">>,
                   game_account_sign = "1",
                   channel_open_id = "1",
                   channel_param = "1",
                   mode = 1,
                   vsn = "1"}.

login_id() ->
    demo_proto_convert:id_mf_convert({acc, login}, id).

