-module(erl_mysql_test_util).

-include_lib("proto_container/include/demo_proto_acc.hrl").

-compile(export_all).
-compile(nowarn_export_all).

connect() ->
    {ok, Ls} = application:get_env(erl_gate, listeners),
    #{ip := IP, port := [Port | _]} = hd(Ls),
    {ok, S} = gen_tcp:connect(IP, Port, []),
    S.

send(S, {ID, Msg}) ->
    {Bin, Size} = demo_proto_c2s:en_packet(ID, Msg),
    gen_tcp:send(S, [<<Size:16>>, Bin]).

login_msg() ->
    ID = demo_proto_convert:id_mf_convert({acc, login}, id),
    Msg = #acc_login_c2s{
        platform = <<"dev">>,
        game_account_id = <<"123123123">>,
        game_account_sign = "1",
        channel_open_id = "3xx1231312323",
        channel_param = "1",
        mode = 1,
        vsn = "1"
    },
    {ID, Msg}.


create_msg() ->
    ID = demo_proto_convert:id_mf_convert({acc, create}, id),
    {ID, #acc_create_c2s{}}.

enter_msg() ->
    ID = demo_proto_convert:id_mf_convert({acc, enter}, id),
    {ID, #acc_enter_c2s{id = 0}}.
