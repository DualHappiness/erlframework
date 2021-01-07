%%%----------------------------------------------
%%%
%%% @doc: 账户信息 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_acc_HRL).
-define(demo_proto_acc_HRL, true).

-record(acc_login_c2s, {platform,game_account_id,game_account_sign,channel_open_id,channel_param,mode,vsn}).
-record(acc_create_c2s, {}).
-record(acc_create_with_params_c2s, {head,sex,name,params}).
-record(acc_enter_c2s, {id}).
-record(acc_relogin_c2s, {platform,game_account_id,game_account_sign,id,channel_param,vsn}).
-record(acc_servertime_c2s, {}).
-record(acc_version_c2s, {vsn}).

-record(acc_login_s2c, {code,id,channel_open_id,game_account_id,game_account_sign,game_login_key}).
-record(acc_create_s2c, {code,id}).
-record(acc_enter_s2c, {code}).
-record(acc_kick_offline_s2c, {content}).
-record(acc_relogin_s2c, {code}).
-record(acc_servertime_s2c, {time}).
-record(acc_ban_s2c, {reason,end_time}).
-record(acc_maintain_s2c, {state,desc}).
-record(acc_version_s2c, {result}).

-endif. % demo_proto_acc_HRL