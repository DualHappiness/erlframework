%%%----------------------------------------------
%%%
%%% @doc: test (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_test).
-compile(export_all).
-compile(nowarn_export_all).
-compile(bin_opt_info).

en_test_part1_c2s({test_part1_c2s, Value}) ->
    [
    <<Value:64/signed>>
    ].


de_test_part1_c2s(Size1, Rest1) ->
    Size2 = Size1 - 8,
    <<DeValue:64/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{test_part1_c2s, DeValue}, Rest2}, Size2}.


en_test_part1_s2c({test_part1_s2c, Value}) ->
    [
    <<Value:64/signed>>
    ].


de_test_part1_s2c(Size1, Rest1) ->
    Size2 = Size1 - 8,
    <<DeValue:64/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{test_part1_s2c, DeValue}, Rest2}, Size2}.

en_test_part2_s2c({test_part2_s2c, Msg}) ->
    [
    (demo_proto_util:en_string(Msg))
    ].


de_test_part2_s2c(Size1, Rest1) ->
    <<MsgLen:16, DeMsg:MsgLen/bytes, Rest2:(Size1 - 2 - MsgLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - MsgLen, 
    {ok, {{test_part2_s2c, DeMsg}, Rest2}, Size2}.

