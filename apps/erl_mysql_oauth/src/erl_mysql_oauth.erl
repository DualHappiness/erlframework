-module(erl_mysql_oauth).

-include_lib("proto_container/include/demo_proto_acc.hrl").
-include_lib("erl_oauth_lib/include/erl_oauth_lib.hrl").
-include_lib("eunit/include/eunit.hrl").

%% TODO relogin
-export([login_validate/2]).

-spec login_validate(Msg :: term(), Args :: term()) -> {ok,
                                                        {oauth_fail, Reason :: term()} |
                                                        {oauth_success, term()}}.
login_validate({_Mod, Req}, Args) ->
    IP = proplists:get_value(ip, Args),
    ?assert(not ?MATCHES(undefine, IP)),
    #acc_login_c2s{platform = PlatformBin, channel_open_id = OpenID} = Req,
    %% TODO 增加safe 防止攻击
    AuthBody = #auth_body{platform = binary_to_atom(PlatformBin, latin1), openid = OpenID},
    case erl_oauth_lib:auth(AuthBody) of
      {error, Reason} ->
          {ok, {oauth_fail, Reason}};
      {ok, Ret} ->
          #auth_ret{platform = Platform, accname = AccName, accsign = Sign} = Ret,
          IsExist = erl_mysql_db_account:is_exist(AccName, Platform),
          case ?IF(IsExist, ok, erl_mysql_db_account:create(AccName, Platform, IP, Sign)) of
            ok ->
                {ok, {oauth_success, Ret}};
            {error, Reason} ->
                {ok, {oauth_fail, Reason}}
          end
    end.

