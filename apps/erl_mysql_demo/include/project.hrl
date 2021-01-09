-ifndef(PROJECT_HRL).
-define(PROJECT, true).

%% 帐号进度状态(用来统计流失率)
-define(ACCOUNT_STATUS_INIT, 0).            % 创建帐号
-define(ACCOUNT_STATUS_LOAD_CREATE, 1).     % 创建角色加载完成
-define(ACCOUNT_STATUS_FINISH_CREATE, 2).   % 创建角色
-define(ACCOUNT_STATUS_ENTER, 3).           % 进入游戏

-define(PLATFORM_MAP, #{
    dev => 0
}).

-endif.