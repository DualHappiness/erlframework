%%%----------------------------------------------
%%%
%%% @doc: lq{ (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_custom_types).
-compile(export_all).
-compile(nowarn_export_all).
-compile(bin_opt_info).

en_p_player({p_player, Id, Name, Sex, Lvl, Ip}) ->
    [
    <<Id:64/signed>>, 
    (demo_proto_util:en_string(Name)), 
    <<Sex:8/signed>>, 
    <<Lvl:32/signed>>, 
    (demo_proto_util:en_string(Ip))
    ].

de_p_player(Size1, Rest1) ->
    Size2 = Size1 - 8,
    <<DeId:64/signed, Rest2:Size2/binary>> = Rest1, 
    <<NameLen:16, DeName:NameLen/bytes, Rest3:(Size2 - 2 - NameLen)/binary>> = Rest2, 
    Size3 = Size2 - 2 - NameLen, 
    Size4 = Size3 - 1,
    <<DeSex:8/signed, Rest4:Size4/binary>> = Rest3, 
    Size5 = Size4 - 4,
    <<DeLvl:32/signed, Rest5:Size5/binary>> = Rest4, 
    <<IpLen:16, DeIp:IpLen/bytes, Rest6:(Size5 - 2 - IpLen)/binary>> = Rest5, 
    Size6 = Size5 - 2 - IpLen, 
    {ok, {{p_player, DeId, DeName, DeSex, DeLvl, DeIp}, Rest6}, Size6}.

en_p_item({p_item, Id, Count}) ->
    [
    <<Id:32/signed>>, 
    <<Count:64/signed>>
    ].

de_p_item(Size1, Rest1) ->
    Size2 = Size1 - 4,
    <<DeId:32/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - 8,
    <<DeCount:64/signed, Rest3:Size3/binary>> = Rest2, 
    {ok, {{p_item, DeId, DeCount}, Rest3}, Size3}.

