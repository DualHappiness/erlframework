-module(db_player).
-include_lib("eunit/include/eunit.hrl").
-include_lib("desc_container/include/datadesc.hrl").
-include_lib("erl_logger/include/logger.hrl").
-include_lib("pmod_transform/include/pmod.hrl").

-export([load/1, is_exist/1]).

-spec load(any()) -> ok.
load(Key) ->
    Db = dataloggersql_helper:get_db(),
    case Db:select("player", fields_str(#player_values{}), where(Key)) of
        {selected, _, []} ->
            pass;
        {selected, _, [Data]} ->
            datatable_player:raw_insert(Key, list_to_tuple([player_values | Data]));
        Err = {error, Reason} ->
            ?ERROR("load failed~nloader: ~p~nkey: ~p~nreason: ~p~n", [?MODULE, Key, Reason]),
            throw(Err)
    end,
    ok.
-spec is_exist(any()) -> boolean().
%% ! 每一条数据使用前一定要确保检查过一次存在
is_exist(Key) ->
    datatable_loader:load(?MODULE, Key),
    not ?MATCHES(undefined, datatable_player:get(Key)).
