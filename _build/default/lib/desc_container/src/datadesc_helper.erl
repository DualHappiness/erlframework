-module(datadesc_helper).

-define(DATADESC_NOIMPORT, true).
-include("datadesc.hrl").
-include_lib("erl_logger/include/logger.hrl").
-export([fields/1, values/1, where/1, fields_str/1]).

fields(Record) ->
    fields_(element(1, Record)).

fields_(rank_keys) ->
    record_info(fields, rank_keys);
fields_(rank_values) ->
    record_info(fields, rank_values);
fields_(account_keys) ->
    record_info(fields, account_keys);
fields_(account_values) ->
    record_info(fields, account_values);
fields_(account2player_keys) ->
    record_info(fields, account2player_keys);
fields_(account2player_values) ->
    record_info(fields, account2player_values);
fields_(player_keys) ->
    record_info(fields, player_keys);
fields_(player_values) ->
    record_info(fields, player_values);
fields_(test_keys) ->
    record_info(fields, test_keys);
fields_(test_values) ->
    record_info(fields, test_values);
fields_(_V) ->
    ?ERROR("wrong record: ~p~n", [_V]).

fields_str(Record) ->
    fields_str_(element(1, Record)).

fields_str_(rank_keys) ->
    ["player_id", "weapon_id"];
fields_str_(rank_values) ->
    ["rank"];

fields_str_(account_keys) ->
    ["accname", "platform"];
fields_str_(account_values) ->
    ["status", "create_time", "create_ip", "token"];

fields_str_(account2player_keys) ->
    ["accname", "platform"];
fields_str_(account2player_values) ->
    ["player_id"];

fields_str_(player_keys) ->
    ["id"];
fields_str_(player_values) ->
    ["sex", "name", "head_img", "last_login_ip"];

fields_str_(test_keys) ->
    ["key1"];
fields_str_(test_values) ->
    ["val1"];
fields_str_(_V) ->
    ?ERROR("wrong record: ~p~n", [_V]).

values(Record) ->
    tl(tuple_to_list(Record)).

%% 单纯考虑性能的话自己手动调用record_info效率会高一点点
where(Record) -> 
    CondList = [[F, "=", mysql:encode(V)] || {F, V} <- lists:zip(fields_str(Record), values(Record))],
    concat_where_(CondList).

concat_where_([H | T]) ->
    concat_where_(T, [H]).
concat_where_([], Result) ->
    Result;
concat_where_([H | T], Result) ->
    concat_where_(T, [H, " and " | Result]).

