-module(datatable_test).

-include_lib("desc_container/include/datadesc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([insert/2, update/2, update/3, delete/1, get/1, raw_insert/2]).

insert(Key, Value) ->
    datatable_general:insert(test, Key, Value).

update(Key, Value) ->
    ?assert(is_record(Key, test_keys)),
    ?assert(is_record(Value, test_values)),
    ?assert(not ?MATCHES([], ets:lookup(test, Key))),
    datatable_general:update(test, Key, Value).

delete(Key) ->
    ?assert(is_record(Key, test_keys)),
    ?assert(not ?MATCHES([], ets:lookup(test, Key))),
    datatable_general:delete(test, Key).

get(Key) ->
    datatable_general:get(test, Key).

raw_insert(Key, Value) ->
    ?assert(is_record(Key, test_keys)),
    ?assert(is_record(Value, test_values)),
    datatable_general:raw_insert(test, Key, Value).

update(Key, Pos, Value) ->
    ?assert(is_record(Key, test_keys)),
    ?assert(is_integer(Pos) andalso Pos > 0),
    datatable_general:update(test, Key, Pos, Value).