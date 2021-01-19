%%%----------------------------------------------
%%%
%%% @doc: 全局信息 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_system).
-compile(export_all).
-compile(nowarn_export_all).
-compile(bin_opt_info).


en_system_error_s2c({system_error_s2c, Msgid, Code}) ->
    [
    <<Msgid:16/signed>>, 
    <<Code:32/signed>>
    ].


de_system_error_s2c(Size1, Rest1) ->
    Size2 = Size1 - 2,
    <<DeMsgid:16/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - 4,
    <<DeCode:32/signed, Rest3:Size3/binary>> = Rest2, 
    {ok, {{system_error_s2c, DeMsgid, DeCode}, Rest3}, Size3}.

