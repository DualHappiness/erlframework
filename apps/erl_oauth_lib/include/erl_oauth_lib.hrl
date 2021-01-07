%% * 统一的认证请求 摆脱消息格式依赖
%% TODO 根据各平台要求完善参数
-record(auth_body, {platform :: atom(), openid, params}).
%% TODO
-record(auth_ret, {platform :: atom(), accname :: binary(), accsign :: binary()}).

