-module(erl_mysql_db_account).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datadescsql_container/include/datadesc.hrl").
-include_lib("erl_logger/include/logger.hrl").
-include_lib("erl_mysql_demo/include/project.hrl").
-include_lib("pmod/include/pmod.hrl").

-export([load/1]).
-export([is_exist/2, create/4]).

-type accname() :: term().
-type platform() :: term().

%% TODO loader 通用实现
%% 正好卡在logger 和 table 之间 所以现在都是放在项目中
%%
-spec load(any()) -> ok.
load(Key) ->
    Db = dataloggersql_helper:get_db(),
    case Db:select("account", fields_str(#account_values{}), where(Key)) of
      {selected, _, []} ->
          pass;
      {selected, _, [Data]} ->
          datatable_account:raw_insert(Key, list_to_tuple([account_values | Data]));
      Err = {error, Reason} ->
          ?ERROR("load failed~nloader: ~p~nkey: ~p~nreason: ~p~n", [?MODULE, Key, Reason]),
          throw(Err)
    end,
    ok.

%% TODO 确认取出来的是啥
-spec is_exist(AccName :: accname(), Platform :: platform()) -> boolean().
is_exist(AccName, Platform) ->
    Key = #account_keys{accname = AccName, platform = Platform},
    datatable_loader:load(?MODULE, Key),
    not ?MATCHES(undefined, datatable_account:get(Key)).

-spec create(AccName :: accname(),
             Platform :: platform(),
             IP :: string(),
             Token :: binary()) -> ok | {error, Reason :: term()}.
create(AccName, Platform, IP, Token) ->
    Now = erlang:system_time(seconds),
    case datatable_account:insert(#account_keys{accname = AccName, platform = Platform},
                                  #account_values{player_id = 0,
                                                  status = ?ACCOUNT_STATUS_INIT,
                                                  create_time = Now,
                                                  create_ip = IP,
                                                  token = Token})
        of
      ok ->
          ok;
      {error, _Reason} ->
          {error, create_account_fail}
    end.

