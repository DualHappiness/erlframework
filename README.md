erl_mysql_demo
=====
# format code
    rebar3 fmt

# WARN
使用datalogger要注意多个节点不要共用数据 否则会不一致
当然越过datatable 直接读写数据库 就可以无视这个问题了


## TODO LIST
- gen_mod 功能
- 单节点测试
- gen_mod remote call
- 多节点测试
- data loader 增加unload
- player_data 定期下线功能
- 通过自定义logger handler && logger filter 实现旧的警告功能
- erl gate 完善
- 完善认证授权回传数据 完善建立角色数据
- 便利性功能添加， 需要注意保持逻辑清晰
- 打点相关
- 操作跨服玩家数据
- 旧的库也加上erlfmt


## FIXME
- 协议以来由于include字符串不能用宏拼接 所以现在都是写死的 有点尴尬

Build
-----

    $ rebar3 compile
