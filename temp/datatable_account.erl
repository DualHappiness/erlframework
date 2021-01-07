-module(datatable_account).

-export([insert/2, update/2, update/3, delete/1, get/1, raw_insert/2]). 

insert(Key, Value) ->
    datatable_general:insert(account, Key, Value).

update(Key, Value) ->
    datatable_general:update(account, Key, Value).

delete(Key) ->
    datatable_general:delete(account, Key).

get(Key) ->
    datatable_general:get(account, Key).

raw_insert(Key, Value) ->
    datatable_general:raw_insert(account, Key, Value).

update(Key, Pos, Value) ->
    datatable_general:update(account, Key, Pos, Value).