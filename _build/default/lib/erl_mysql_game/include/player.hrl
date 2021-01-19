%% player session data
-record(player, {id, client, data_pid = undefined, data_ref}).
