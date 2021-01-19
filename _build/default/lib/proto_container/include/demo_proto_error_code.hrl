%%%----------------------------------------------
%%%
%%% @doc:  (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_ERROR_CODE_HRL).
-define(demo_proto_ERROR_CODE_HRL, true).

-define(E_OK                                              ,      0). % 成功
-define(E_UNKNOWN                                         ,      1). % 未知错误
-define(E_FAILED                                          ,      5). % 失败
-define(E_NOT_SUPPORT_VSN                                 ,      6). % 新版本已经准备好，请重启应用哦~
-define(E_SYSTEM                                          ,   1000). % 系统错误，请检查代码逻辑
-define(E_PLAYER_ALREADY_CREATED                          ,   1001). % 角色已创建
-define(E_PLAYER_NO_ROLE                                  ,   1002). % 没有角色
-define(E_PLAYER_ALREADY_ENTER                            ,   1003). % 重复的进入消息
-define(E_PLAYER_KICK_BY_OTHER                            ,   1004). % 由于其他玩家登录，被踢下线
-define(E_PLAYER_NEED_ENTER                               ,   1005). % 未进入就在发后续消息
-define(E_PLAYER_TERMINATE                                ,   1100). % player server 退出
-define(E_MOD_NO_HANDLER                                  ,   2000). % 没有找到处理消息的module

-define(CODE_DESC, [
    {0     , "E_OK"},
    {1     , "E_UNKNOWN"},
    {5     , "E_FAILED"},
    {6     , "E_NOT_SUPPORT_VSN"},
    {1000  , "E_SYSTEM"},
    {1001  , "E_PLAYER_ALREADY_CREATED"},
    {1002  , "E_PLAYER_NO_ROLE"},
    {1003  , "E_PLAYER_ALREADY_ENTER"},
    {1004  , "E_PLAYER_KICK_BY_OTHER"},
    {1005  , "E_PLAYER_NEED_ENTER"},
    {1100  , "E_PLAYER_TERMINATE"},
    {2000  , "E_MOD_NO_HANDLER"},
    {-1, ""}
]).
-endif. % demo_proto_ERROR_CODE_HRL