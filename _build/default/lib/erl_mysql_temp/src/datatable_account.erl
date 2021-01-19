-module(datatable_account).

-include_lib("desc_container/include/datadesc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([insert/2, update/2, update/3, delete/1, get/1, raw_insert/2]).

insert(Key, Value) ->
    datatable_general:insert(account, Key, Value).

update(Key, Value) ->
    ?assert(is_record(Key, account_keys)),
    ?assert(is_record(Value, account_values)),
    ?assert(not ?MATCHES([], ets:lookup(account, Key))),
    datatable_general:update(account, Key, Value).

delete(Key) ->
    ?assert(is_record(Key, account_keys)),
    ?assert(not ?MATCHES([], ets:lookup(account, Key))),
    datatable_general:delete(account, Key).

get(Key) ->
    datatable_general:get(account, Key).

raw_insert(Key, Value) ->
    ?assert(is_record(Key, account_keys)),
    ?assert(is_record(Value, account_values)),
    datatable_general:raw_insert(account, Key, Value).

update(Key, Pos, Value) ->
    ?assert(is_record(Key, account_keys)),
    ?assert(is_integer(Pos) andalso Pos > 0),
    datatable_general:update(account, Key, Pos, Value).