-module(dataloggersql_helper).
-compile(export_all).
-compile(nowarn_export_all).

get_db() ->
   {mysql_db,erl_mysql_demo_db}.