%%%----------------------------------------------
%%%
%%% @doc: 社交服务协议 (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_friend_service).
-compile(export_all).
-compile(nowarn_export_all).
-compile(bin_opt_info).

en_friend_service_register_c2s({friend_service_register_c2s}) ->
    [

    ].


de_friend_service_register_c2s(Size1, Rest1) ->
    {ok, {{friend_service_register_c2s}, Rest1}, Size1}.

en_friend_service_unregister_c2s({friend_service_unregister_c2s}) ->
    [

    ].


de_friend_service_unregister_c2s(Size1, Rest1) ->
    {ok, {{friend_service_unregister_c2s}, Rest1}, Size1}.

en_friend_service_join_c2s({friend_service_join_c2s}) ->
    [

    ].


de_friend_service_join_c2s(Size1, Rest1) ->
    {ok, {{friend_service_join_c2s}, Rest1}, Size1}.

en_friend_service_left_c2s({friend_service_left_c2s}) ->
    [

    ].


de_friend_service_left_c2s(Size1, Rest1) ->
    {ok, {{friend_service_left_c2s}, Rest1}, Size1}.

en_friend_service_send_c2s({friend_service_send_c2s}) ->
    [

    ].


de_friend_service_send_c2s(Size1, Rest1) ->
    {ok, {{friend_service_send_c2s}, Rest1}, Size1}.


en_friend_service_register_s2c({friend_service_register_s2c, User_id, Agent_type, Agent_addr}) ->
    [
    (demo_proto_util:en_string(User_id)), 
    (demo_proto_util:en_string(Agent_type)), 
    (demo_proto_util:en_string(Agent_addr))
    ].


de_friend_service_register_s2c(Size1, Rest1) ->
    <<User_idLen:16, DeUser_id:User_idLen/bytes, Rest2:(Size1 - 2 - User_idLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - User_idLen, 
    <<Agent_typeLen:16, DeAgent_type:Agent_typeLen/bytes, Rest3:(Size2 - 2 - Agent_typeLen)/binary>> = Rest2, 
    Size3 = Size2 - 2 - Agent_typeLen, 
    <<Agent_addrLen:16, DeAgent_addr:Agent_addrLen/bytes, Rest4:(Size3 - 2 - Agent_addrLen)/binary>> = Rest3, 
    Size4 = Size3 - 2 - Agent_addrLen, 
    {ok, {{friend_service_register_s2c, DeUser_id, DeAgent_type, DeAgent_addr}, Rest4}, Size4}.

en_friend_service_unregister_s2c({friend_service_unregister_s2c, User_id}) ->
    [
    (demo_proto_util:en_string(User_id))
    ].


de_friend_service_unregister_s2c(Size1, Rest1) ->
    <<User_idLen:16, DeUser_id:User_idLen/bytes, Rest2:(Size1 - 2 - User_idLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - User_idLen, 
    {ok, {{friend_service_unregister_s2c, DeUser_id}, Rest2}, Size2}.

en_friend_service_join_s2c({friend_service_join_s2c, Type, Channel_id, User_id}) ->
    [
    (demo_proto_util:en_string(Type)), 
    (demo_proto_util:en_string(Channel_id)), 
    (demo_proto_util:en_string(User_id))
    ].


de_friend_service_join_s2c(Size1, Rest1) ->
    <<TypeLen:16, DeType:TypeLen/bytes, Rest2:(Size1 - 2 - TypeLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - TypeLen, 
    <<Channel_idLen:16, DeChannel_id:Channel_idLen/bytes, Rest3:(Size2 - 2 - Channel_idLen)/binary>> = Rest2, 
    Size3 = Size2 - 2 - Channel_idLen, 
    <<User_idLen:16, DeUser_id:User_idLen/bytes, Rest4:(Size3 - 2 - User_idLen)/binary>> = Rest3, 
    Size4 = Size3 - 2 - User_idLen, 
    {ok, {{friend_service_join_s2c, DeType, DeChannel_id, DeUser_id}, Rest4}, Size4}.

en_friend_service_left_s2c({friend_service_left_s2c, Type, Channel_id, User_id}) ->
    [
    (demo_proto_util:en_string(Type)), 
    (demo_proto_util:en_string(Channel_id)), 
    (demo_proto_util:en_string(User_id))
    ].


de_friend_service_left_s2c(Size1, Rest1) ->
    <<TypeLen:16, DeType:TypeLen/bytes, Rest2:(Size1 - 2 - TypeLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - TypeLen, 
    <<Channel_idLen:16, DeChannel_id:Channel_idLen/bytes, Rest3:(Size2 - 2 - Channel_idLen)/binary>> = Rest2, 
    Size3 = Size2 - 2 - Channel_idLen, 
    <<User_idLen:16, DeUser_id:User_idLen/bytes, Rest4:(Size3 - 2 - User_idLen)/binary>> = Rest3, 
    Size4 = Size3 - 2 - User_idLen, 
    {ok, {{friend_service_left_s2c, DeType, DeChannel_id, DeUser_id}, Rest4}, Size4}.

en_friend_service_send_s2c({friend_service_send_s2c, Type, To_id, From_id, Data}) ->
    [
    (demo_proto_util:en_string(Type)), 
    (demo_proto_util:en_string(To_id)), 
    (demo_proto_util:en_string(From_id)), 
    (demo_proto_util:en_string(Data))
    ].


de_friend_service_send_s2c(Size1, Rest1) ->
    <<TypeLen:16, DeType:TypeLen/bytes, Rest2:(Size1 - 2 - TypeLen)/binary>> = Rest1, 
    Size2 = Size1 - 2 - TypeLen, 
    <<To_idLen:16, DeTo_id:To_idLen/bytes, Rest3:(Size2 - 2 - To_idLen)/binary>> = Rest2, 
    Size3 = Size2 - 2 - To_idLen, 
    <<From_idLen:16, DeFrom_id:From_idLen/bytes, Rest4:(Size3 - 2 - From_idLen)/binary>> = Rest3, 
    Size4 = Size3 - 2 - From_idLen, 
    <<DataLen:16, DeData:DataLen/bytes, Rest5:(Size4 - 2 - DataLen)/binary>> = Rest4, 
    Size5 = Size4 - 2 - DataLen, 
    {ok, {{friend_service_send_s2c, DeType, DeTo_id, DeFrom_id, DeData}, Rest5}, Size5}.

