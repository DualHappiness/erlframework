-ifndef(DATADESC_HRL).
-define(DATADESC_HRL, true).

-define(TABLE_NAMES, [rank, account, account2player, player]).
-record(rank_keys, {player_id, weapon_id}).
-record(rank_values, {rank}).
-record(rank_all, {player_id, weapon_id, rank}).

-record(account_keys, {accname, platform}).
-record(account_values, {status, create_time, create_ip, token}).
-record(account_all, {accname, platform, status, create_time, create_ip, token}).

-record(account2player_keys, {accname, platform}).
-record(account2player_values, {player_id}).
-record(account2player_all, {accname, platform, player_id}).

-record(player_keys, {id}).
-record(player_values, {sex, name, head_img, last_login_ip}).
-record(player_all, {id, sex, name, head_img, last_login_ip}).


-ifndef(DATADESC_NOIMPORT).
-import(datadesc_helper, [fields/1, values/1, where/1, fields_str/1]).
-inline([fields/1, values/1, where/1, fields_str]).
-endif.
-endif.