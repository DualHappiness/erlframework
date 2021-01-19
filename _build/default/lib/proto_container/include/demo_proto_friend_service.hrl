%%%----------------------------------------------
%%%
%%% @doc: 社交服务协议 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_friend_service_HRL).
-define(demo_proto_friend_service_HRL, true).

-record(friend_service_register_c2s, {}).
-record(friend_service_unregister_c2s, {}).
-record(friend_service_join_c2s, {}).
-record(friend_service_left_c2s, {}).
-record(friend_service_send_c2s, {}).

-record(friend_service_register_s2c, {user_id :: iolist() | atom() | binary() ,agent_type :: iolist() | atom() | binary() ,agent_addr :: iolist() | atom() | binary() }).
-record(friend_service_unregister_s2c, {user_id :: iolist() | atom() | binary() }).
-record(friend_service_join_s2c, {type :: iolist() | atom() | binary() ,channel_id :: iolist() | atom() | binary() ,user_id :: iolist() | atom() | binary() }).
-record(friend_service_left_s2c, {type :: iolist() | atom() | binary() ,channel_id :: iolist() | atom() | binary() ,user_id :: iolist() | atom() | binary() }).
-record(friend_service_send_s2c, {type :: iolist() | atom() | binary() ,to_id :: iolist() | atom() | binary() ,from_id :: iolist() | atom() | binary() ,data :: iolist() | atom() | binary() }).

-endif. % demo_proto_friend_service_HRL