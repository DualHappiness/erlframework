%%%----------------------------------------------
%%%
%%% @doc: 账户信息 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_acc).
-compile(export_all).
-compile(nowarn_export_all).
-compile(bin_opt_info).

en_acc_login_c2s({acc_login_c2s, Platform, Game_account_id, Game_account_sign, Channel_open_id, Channel_param, Mode, Vsn}) ->
    [
    (demo_proto_util:en_string(Platform)), 
    (demo_proto_util:en_string(Game_account_id)), 
    (demo_proto_util:en_string(Game_account_sign)), 
    (demo_proto_util:en_string(Channel_open_id)), 
    (demo_proto_util:en_string(Channel_param)), 
    <<Mode:8/signed>>, 
    (demo_proto_util:en_string(Vsn))
    ].


de_acc_login_c2s(Size1, Rest1) ->
    <<PlatformLen:16, DePlatform:PlatformLen/bytes, Rest2:(Size1 - 16 - PlatformLen)/binary>> = Rest1, 
    Size2 = Size1 - 16 - PlatformLen, 
    <<Game_account_idLen:16, DeGame_account_id:Game_account_idLen/bytes, Rest3:(Size2 - 16 - Game_account_idLen)/binary>> = Rest2, 
    Size3 = Size2 - 16 - Game_account_idLen, 
    <<Game_account_signLen:16, DeGame_account_sign:Game_account_signLen/bytes, Rest4:(Size3 - 16 - Game_account_signLen)/binary>> = Rest3, 
    Size4 = Size3 - 16 - Game_account_signLen, 
    <<Channel_open_idLen:16, DeChannel_open_id:Channel_open_idLen/bytes, Rest5:(Size4 - 16 - Channel_open_idLen)/binary>> = Rest4, 
    Size5 = Size4 - 16 - Channel_open_idLen, 
    <<Channel_paramLen:16, DeChannel_param:Channel_paramLen/bytes, Rest6:(Size5 - 16 - Channel_paramLen)/binary>> = Rest5, 
    Size6 = Size5 - 16 - Channel_paramLen, 
    Size7 = Size6 - "8/signed",
    <<DeMode:8/signed, Rest7:Size7/binary>> = Rest6, 
    <<VsnLen:16, DeVsn:VsnLen/bytes, Rest8:(Size7 - 16 - VsnLen)/binary>> = Rest7, 
    Size8 = Size7 - 16 - VsnLen, 
    {ok, {{acc_login_c2s, DePlatform, DeGame_account_id, DeGame_account_sign, DeChannel_open_id, DeChannel_param, DeMode, DeVsn}, Rest8}, Size8}.

en_acc_create_c2s({acc_create_c2s}) ->
    [

    ].


de_acc_create_c2s(Size1, Rest1) ->
    {ok, {{acc_create_c2s}, Rest1}, Size1}.

en_acc_create_with_params_c2s({acc_create_with_params_c2s, Head, Sex, Name, Params}) ->
    [
    <<Head:8/signed>>, 
    <<Sex:8/signed>>, 
    (demo_proto_util:en_string(Name)), 
    (demo_proto_util:en_string(Params))
    ].


de_acc_create_with_params_c2s(Size1, Rest1) ->
    Size2 = Size1 - "8/signed",
    <<DeHead:8/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - "8/signed",
    <<DeSex:8/signed, Rest3:Size3/binary>> = Rest2, 
    <<NameLen:16, DeName:NameLen/bytes, Rest4:(Size3 - 16 - NameLen)/binary>> = Rest3, 
    Size4 = Size3 - 16 - NameLen, 
    <<ParamsLen:16, DeParams:ParamsLen/bytes, Rest5:(Size4 - 16 - ParamsLen)/binary>> = Rest4, 
    Size5 = Size4 - 16 - ParamsLen, 
    {ok, {{acc_create_with_params_c2s, DeHead, DeSex, DeName, DeParams}, Rest5}, Size5}.

en_acc_enter_c2s({acc_enter_c2s, Id}) ->
    [
    <<Id:64/signed>>
    ].


de_acc_enter_c2s(Size1, Rest1) ->
    Size2 = Size1 - "64/signed",
    <<DeId:64/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{acc_enter_c2s, DeId}, Rest2}, Size2}.

en_acc_relogin_c2s({acc_relogin_c2s, Platform, Game_account_id, Game_account_sign, Id, Channel_param, Vsn}) ->
    [
    (demo_proto_util:en_string(Platform)), 
    (demo_proto_util:en_string(Game_account_id)), 
    (demo_proto_util:en_string(Game_account_sign)), 
    <<Id:64/signed>>, 
    (demo_proto_util:en_string(Channel_param)), 
    (demo_proto_util:en_string(Vsn))
    ].


de_acc_relogin_c2s(Size1, Rest1) ->
    <<PlatformLen:16, DePlatform:PlatformLen/bytes, Rest2:(Size1 - 16 - PlatformLen)/binary>> = Rest1, 
    Size2 = Size1 - 16 - PlatformLen, 
    <<Game_account_idLen:16, DeGame_account_id:Game_account_idLen/bytes, Rest3:(Size2 - 16 - Game_account_idLen)/binary>> = Rest2, 
    Size3 = Size2 - 16 - Game_account_idLen, 
    <<Game_account_signLen:16, DeGame_account_sign:Game_account_signLen/bytes, Rest4:(Size3 - 16 - Game_account_signLen)/binary>> = Rest3, 
    Size4 = Size3 - 16 - Game_account_signLen, 
    Size5 = Size4 - "64/signed",
    <<DeId:64/signed, Rest5:Size5/binary>> = Rest4, 
    <<Channel_paramLen:16, DeChannel_param:Channel_paramLen/bytes, Rest6:(Size5 - 16 - Channel_paramLen)/binary>> = Rest5, 
    Size6 = Size5 - 16 - Channel_paramLen, 
    <<VsnLen:16, DeVsn:VsnLen/bytes, Rest7:(Size6 - 16 - VsnLen)/binary>> = Rest6, 
    Size7 = Size6 - 16 - VsnLen, 
    {ok, {{acc_relogin_c2s, DePlatform, DeGame_account_id, DeGame_account_sign, DeId, DeChannel_param, DeVsn}, Rest7}, Size7}.

en_acc_servertime_c2s({acc_servertime_c2s}) ->
    [

    ].


de_acc_servertime_c2s(Size1, Rest1) ->
    {ok, {{acc_servertime_c2s}, Rest1}, Size1}.

en_acc_version_c2s({acc_version_c2s, Vsn}) ->
    [
    (demo_proto_util:en_string(Vsn))
    ].


de_acc_version_c2s(Size1, Rest1) ->
    <<VsnLen:16, DeVsn:VsnLen/bytes, Rest2:(Size1 - 16 - VsnLen)/binary>> = Rest1, 
    Size2 = Size1 - 16 - VsnLen, 
    {ok, {{acc_version_c2s, DeVsn}, Rest2}, Size2}.


en_acc_login_s2c({acc_login_s2c, Code, Id, Channel_open_id, Game_account_id, Game_account_sign, Game_login_key}) ->
    [
    <<Code:16/signed>>, 
    <<Id:64/signed>>, 
    (demo_proto_util:en_string(Channel_open_id)), 
    (demo_proto_util:en_string(Game_account_id)), 
    (demo_proto_util:en_string(Game_account_sign)), 
    (demo_proto_util:en_string(Game_login_key))
    ].


de_acc_login_s2c(Size1, Rest1) ->
    Size2 = Size1 - "16/signed",
    <<DeCode:16/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - "64/signed",
    <<DeId:64/signed, Rest3:Size3/binary>> = Rest2, 
    <<Channel_open_idLen:16, DeChannel_open_id:Channel_open_idLen/bytes, Rest4:(Size3 - 16 - Channel_open_idLen)/binary>> = Rest3, 
    Size4 = Size3 - 16 - Channel_open_idLen, 
    <<Game_account_idLen:16, DeGame_account_id:Game_account_idLen/bytes, Rest5:(Size4 - 16 - Game_account_idLen)/binary>> = Rest4, 
    Size5 = Size4 - 16 - Game_account_idLen, 
    <<Game_account_signLen:16, DeGame_account_sign:Game_account_signLen/bytes, Rest6:(Size5 - 16 - Game_account_signLen)/binary>> = Rest5, 
    Size6 = Size5 - 16 - Game_account_signLen, 
    <<Game_login_keyLen:16, DeGame_login_key:Game_login_keyLen/bytes, Rest7:(Size6 - 16 - Game_login_keyLen)/binary>> = Rest6, 
    Size7 = Size6 - 16 - Game_login_keyLen, 
    {ok, {{acc_login_s2c, DeCode, DeId, DeChannel_open_id, DeGame_account_id, DeGame_account_sign, DeGame_login_key}, Rest7}, Size7}.

en_acc_create_s2c({acc_create_s2c, Code, Id}) ->
    [
    <<Code:16/signed>>, 
    <<Id:64/signed>>
    ].


de_acc_create_s2c(Size1, Rest1) ->
    Size2 = Size1 - "16/signed",
    <<DeCode:16/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - "64/signed",
    <<DeId:64/signed, Rest3:Size3/binary>> = Rest2, 
    {ok, {{acc_create_s2c, DeCode, DeId}, Rest3}, Size3}.

en_acc_enter_s2c({acc_enter_s2c, Code}) ->
    [
    <<Code:16/signed>>
    ].


de_acc_enter_s2c(Size1, Rest1) ->
    Size2 = Size1 - "16/signed",
    <<DeCode:16/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{acc_enter_s2c, DeCode}, Rest2}, Size2}.

en_acc_kick_offline_s2c({acc_kick_offline_s2c, Content}) ->
    [
    (demo_proto_util:en_string(Content))
    ].


de_acc_kick_offline_s2c(Size1, Rest1) ->
    <<ContentLen:16, DeContent:ContentLen/bytes, Rest2:(Size1 - 16 - ContentLen)/binary>> = Rest1, 
    Size2 = Size1 - 16 - ContentLen, 
    {ok, {{acc_kick_offline_s2c, DeContent}, Rest2}, Size2}.

en_acc_relogin_s2c({acc_relogin_s2c, Code}) ->
    [
    <<Code:16/signed>>
    ].


de_acc_relogin_s2c(Size1, Rest1) ->
    Size2 = Size1 - "16/signed",
    <<DeCode:16/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{acc_relogin_s2c, DeCode}, Rest2}, Size2}.

en_acc_servertime_s2c({acc_servertime_s2c, Time}) ->
    [
    <<Time:32/signed>>
    ].


de_acc_servertime_s2c(Size1, Rest1) ->
    Size2 = Size1 - "32/signed",
    <<DeTime:32/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{acc_servertime_s2c, DeTime}, Rest2}, Size2}.

en_acc_ban_s2c({acc_ban_s2c, Reason, End_time}) ->
    [
    <<Reason:8/signed>>, 
    <<End_time:32/signed>>
    ].


de_acc_ban_s2c(Size1, Rest1) ->
    Size2 = Size1 - "8/signed",
    <<DeReason:8/signed, Rest2:Size2/binary>> = Rest1, 
    Size3 = Size2 - "32/signed",
    <<DeEnd_time:32/signed, Rest3:Size3/binary>> = Rest2, 
    {ok, {{acc_ban_s2c, DeReason, DeEnd_time}, Rest3}, Size3}.

en_acc_maintain_s2c({acc_maintain_s2c, State, Desc}) ->
    [
    <<State:16/signed>>, 
    (demo_proto_util:en_string(Desc))
    ].


de_acc_maintain_s2c(Size1, Rest1) ->
    Size2 = Size1 - "16/signed",
    <<DeState:16/signed, Rest2:Size2/binary>> = Rest1, 
    <<DescLen:16, DeDesc:DescLen/bytes, Rest3:(Size2 - 16 - DescLen)/binary>> = Rest2, 
    Size3 = Size2 - 16 - DescLen, 
    {ok, {{acc_maintain_s2c, DeState, DeDesc}, Rest3}, Size3}.

en_acc_version_s2c({acc_version_s2c, Result}) ->
    [
    <<Result:8/signed>>
    ].


de_acc_version_s2c(Size1, Rest1) ->
    Size2 = Size1 - "8/signed",
    <<DeResult:8/signed, Rest2:Size2/binary>> = Rest1, 
    {ok, {{acc_version_s2c, DeResult}, Rest2}, Size2}.

