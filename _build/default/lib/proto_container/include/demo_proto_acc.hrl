%%%----------------------------------------------
%%%
%%% @doc: 账户信息 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_acc_HRL).
-define(demo_proto_acc_HRL, true).

-record(acc_login_c2s, {platform :: iolist() | atom() | binary() ,game_account_id :: iolist() | atom() | binary() ,game_account_sign :: iolist() | atom() | binary() ,channel_open_id :: iolist() | atom() | binary() ,channel_param :: iolist() | atom() | binary() ,mode :: non_neg_integer() ,vsn :: iolist() | atom() | binary() }).
-record(acc_create_c2s, {}).
-record(acc_enter_c2s, {id :: non_neg_integer() }).
-record(acc_relogin_c2s, {platform :: iolist() | atom() | binary() ,game_account_id :: iolist() | atom() | binary() ,game_account_sign :: iolist() | atom() | binary() ,id :: non_neg_integer() ,channel_param :: iolist() | atom() | binary() ,vsn :: iolist() | atom() | binary() }).

-record(acc_login_s2c, {code :: non_neg_integer() ,id :: non_neg_integer() ,channel_open_id :: iolist() | atom() | binary() ,game_account_id :: iolist() | atom() | binary() ,game_account_sign :: iolist() | atom() | binary() ,game_login_key :: iolist() | atom() | binary() }).
-record(acc_create_s2c, {id :: non_neg_integer() }).
-record(acc_enter_s2c, {code :: non_neg_integer() }).
-record(acc_kick_offline_s2c, {content :: iolist() | atom() | binary() }).
-record(acc_relogin_s2c, {code :: non_neg_integer() }).

-endif. % demo_proto_acc_HRL