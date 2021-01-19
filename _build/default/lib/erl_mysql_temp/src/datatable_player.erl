-module(datatable_player).

-include_lib("desc_container/include/datadesc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([insert/2, update/2, update/3, delete/1, get/1, raw_insert/2]).

insert(Key, Value) ->
    datatable_general:insert(player, Key, Value).

update(Key, Value) ->
    ?assert(is_record(Key, player_keys)),
    ?assert(is_record(Value, player_values)),
    ?assert(not ?MATCHES([], ets:lookup(player, Key))),
    datatable_general:update(player, Key, Value).

delete(Key) ->
    ?assert(is_record(Key, player_keys)),
    ?assert(not ?MATCHES([], ets:lookup(player, Key))),
    datatable_general:delete(player, Key).

get(Key) ->
    datatable_general:get(player, Key).

raw_insert(Key, Value) ->
    ?assert(is_record(Key, player_keys)),
    ?assert(is_record(Value, player_values)),
    datatable_general:raw_insert(player, Key, Value).

update(Key, Pos, Value) ->
    ?assert(is_record(Key, player_keys)),
    ?assert(is_integer(Pos) andalso Pos > 0),
    datatable_general:update(player, Key, Pos, Value).