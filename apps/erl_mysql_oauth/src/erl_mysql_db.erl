-module(erl_mysql_db).

-include_lib("datatable/include/datatable.hrl").
-include_lib("datadescsql_container/include/datadesc.hrl").
-include_lib("erl_logger/include/logger.hrl").
-export([start/0]).

-spec start() -> ok.
start() ->
    TableDeclares = [#datatable_declare{table = account}],
    ?DEBUG("new data table: ~p~n", [TableDeclares]),
    datatable:start(TableDeclares),
    ok.
