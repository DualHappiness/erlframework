erl_mysql_demo
=====
# format code
    rebar3 fmt

# WARN
使用datalogger要注意多个节点不要共用数据 否则会不一致
当然越过datatable 直接读写数据库 就可以无视这个问题了


## TODO LIST
- 单节点测试
- 多节点测试
- 通过自定义logger handler && logger filter 实现旧的警告功能
- 完善认证授权回传数据 完善建立角色数据
- 便利性功能添加， 需要注意保持逻辑清晰
- 打点相关
- 操作跨服玩家数据
- 旧的库也加上erlfmt


Build
-----

    $ rebar3 compile
