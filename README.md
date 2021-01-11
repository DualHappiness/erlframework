erl_mysql_demo
=====

使用datalogger要注意多个节点不要共用数据 否则会不一致
当然越过datatable 直接读写数据库 就可以无视这个问题了


## TODO LIST
- 通过自定义logger handler && logger filter 实现旧的警告功能
- 打点相关
- 操作跨服玩家数据

Build
-----

    $ rebar3 compile
