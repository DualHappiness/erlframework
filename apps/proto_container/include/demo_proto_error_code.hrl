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

-define(CODE_DESC, [
    {0     , "E_OK"},
    {1     , "E_UNKNOWN"},
    {5     , "E_FAILED"},
    {6     , "E_NOT_SUPPORT_VSN"},
    {-1, ""}
]).
-endif. % demo_proto_ERROR_CODE_HRL