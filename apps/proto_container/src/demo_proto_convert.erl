%%%----------------------------------------------
%%%
%%% @doc: O®idlb (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_convert).
-export([id_mf_convert/2]).

id_mf_convert(10, _) -> demo_proto_acc; 
id_mf_convert(acc, _) -> 10; 
id_mf_convert({acc, login}, id) -> {10, 0}; 
id_mf_convert({10, 0}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_login_c2s}; 
id_mf_convert({10, 0}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_login_s2c}; 
id_mf_convert({10, 0}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_login_c2s}; 
id_mf_convert({10, 0}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_login_s2c}; 
id_mf_convert({acc, create}, id) -> {10, 2}; 
id_mf_convert({10, 2}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_create_c2s}; 
id_mf_convert({10, 2}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_create_s2c}; 
id_mf_convert({10, 2}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_create_c2s}; 
id_mf_convert({10, 2}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_create_s2c}; 
id_mf_convert({acc, create_with_params}, id) -> {10, 3}; 
id_mf_convert({10, 3}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_create_with_params_c2s}; 
id_mf_convert({10, 3}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_create_with_params_s2c}; 
id_mf_convert({10, 3}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_create_with_params_c2s}; 
id_mf_convert({10, 3}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_create_with_params_s2c}; 
id_mf_convert({acc, enter}, id) -> {10, 4}; 
id_mf_convert({10, 4}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_enter_c2s}; 
id_mf_convert({10, 4}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_enter_s2c}; 
id_mf_convert({10, 4}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_enter_c2s}; 
id_mf_convert({10, 4}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_enter_s2c}; 
id_mf_convert({acc, kick_offline}, id) -> {10, 5}; 
id_mf_convert({10, 5}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_kick_offline_c2s}; 
id_mf_convert({10, 5}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_kick_offline_s2c}; 
id_mf_convert({10, 5}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_kick_offline_c2s}; 
id_mf_convert({10, 5}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_kick_offline_s2c}; 
id_mf_convert({acc, relogin}, id) -> {10, 6}; 
id_mf_convert({10, 6}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_relogin_c2s}; 
id_mf_convert({10, 6}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_relogin_s2c}; 
id_mf_convert({10, 6}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_relogin_c2s}; 
id_mf_convert({10, 6}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_relogin_s2c}; 
id_mf_convert({acc, servertime}, id) -> {10, 7}; 
id_mf_convert({10, 7}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_servertime_c2s}; 
id_mf_convert({10, 7}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_servertime_s2c}; 
id_mf_convert({10, 7}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_servertime_c2s}; 
id_mf_convert({10, 7}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_servertime_s2c}; 
id_mf_convert({acc, ban}, id) -> {10, 10}; 
id_mf_convert({10, 10}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_ban_c2s}; 
id_mf_convert({10, 10}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_ban_s2c}; 
id_mf_convert({10, 10}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_ban_c2s}; 
id_mf_convert({10, 10}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_ban_s2c}; 
id_mf_convert({acc, maintain}, id) -> {10, 11}; 
id_mf_convert({10, 11}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_maintain_c2s}; 
id_mf_convert({10, 11}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_maintain_s2c}; 
id_mf_convert({10, 11}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_maintain_c2s}; 
id_mf_convert({10, 11}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_maintain_s2c}; 
id_mf_convert({acc, version}, id) -> {10, 12}; 
id_mf_convert({10, 12}, {en,c2s}) -> {acc, demo_proto_acc, en_acc_version_c2s}; 
id_mf_convert({10, 12}, {en,s2c}) -> {acc, demo_proto_acc, en_acc_version_s2c}; 
id_mf_convert({10, 12}, {de,c2s}) -> {acc, demo_proto_acc, de_acc_version_c2s}; 
id_mf_convert({10, 12}, {de,s2c}) -> {acc, demo_proto_acc, de_acc_version_s2c}; 
id_mf_convert(201, _) -> demo_proto_friend_service; 
id_mf_convert(friend_service, _) -> 201; 
id_mf_convert({friend_service, register}, id) -> {201, 0}; 
id_mf_convert({201, 0}, {en,c2s}) -> {friend_service, demo_proto_friend_service, en_friend_service_register_c2s}; 
id_mf_convert({201, 0}, {en,s2c}) -> {friend_service, demo_proto_friend_service, en_friend_service_register_s2c}; 
id_mf_convert({201, 0}, {de,c2s}) -> {friend_service, demo_proto_friend_service, de_friend_service_register_c2s}; 
id_mf_convert({201, 0}, {de,s2c}) -> {friend_service, demo_proto_friend_service, de_friend_service_register_s2c}; 
id_mf_convert({friend_service, unregister}, id) -> {201, 1}; 
id_mf_convert({201, 1}, {en,c2s}) -> {friend_service, demo_proto_friend_service, en_friend_service_unregister_c2s}; 
id_mf_convert({201, 1}, {en,s2c}) -> {friend_service, demo_proto_friend_service, en_friend_service_unregister_s2c}; 
id_mf_convert({201, 1}, {de,c2s}) -> {friend_service, demo_proto_friend_service, de_friend_service_unregister_c2s}; 
id_mf_convert({201, 1}, {de,s2c}) -> {friend_service, demo_proto_friend_service, de_friend_service_unregister_s2c}; 
id_mf_convert({friend_service, join}, id) -> {201, 2}; 
id_mf_convert({201, 2}, {en,c2s}) -> {friend_service, demo_proto_friend_service, en_friend_service_join_c2s}; 
id_mf_convert({201, 2}, {en,s2c}) -> {friend_service, demo_proto_friend_service, en_friend_service_join_s2c}; 
id_mf_convert({201, 2}, {de,c2s}) -> {friend_service, demo_proto_friend_service, de_friend_service_join_c2s}; 
id_mf_convert({201, 2}, {de,s2c}) -> {friend_service, demo_proto_friend_service, de_friend_service_join_s2c}; 
id_mf_convert({friend_service, left}, id) -> {201, 3}; 
id_mf_convert({201, 3}, {en,c2s}) -> {friend_service, demo_proto_friend_service, en_friend_service_left_c2s}; 
id_mf_convert({201, 3}, {en,s2c}) -> {friend_service, demo_proto_friend_service, en_friend_service_left_s2c}; 
id_mf_convert({201, 3}, {de,c2s}) -> {friend_service, demo_proto_friend_service, de_friend_service_left_c2s}; 
id_mf_convert({201, 3}, {de,s2c}) -> {friend_service, demo_proto_friend_service, de_friend_service_left_s2c}; 
id_mf_convert({friend_service, send}, id) -> {201, 4}; 
id_mf_convert({201, 4}, {en,c2s}) -> {friend_service, demo_proto_friend_service, en_friend_service_send_c2s}; 
id_mf_convert({201, 4}, {en,s2c}) -> {friend_service, demo_proto_friend_service, en_friend_service_send_s2c}; 
id_mf_convert({201, 4}, {de,c2s}) -> {friend_service, demo_proto_friend_service, de_friend_service_send_c2s}; 
id_mf_convert({201, 4}, {de,s2c}) -> {friend_service, demo_proto_friend_service, de_friend_service_send_s2c}; 
id_mf_convert(_, _) -> 
throw(erro_convert).