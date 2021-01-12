%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%     多读单写来保证性能和数据唯一
%%% @end
%%% Created : 2021/01/08
%%%-------------------------------------------------------------------
-module(player_server_mgr).

-behaviour(gen_server).

-record(state, {tab, monitors = #{}}).

-include_lib("erl_logger/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("proto_container/include/demo_proto_acc.hrl").
-include_lib("proto_container/include/demo_proto_system.hrl").
-include_lib("proto_container/include/demo_proto_error_code.hrl").
-include_lib("desc_container/include/datadesc.hrl").
-include_lib("erl_oauth_lib/include/erl_oauth_lib.hrl").
-include_lib("erl_mysql_demo/include/project.hrl").

-type msg() :: {#auth_ret{}, {Mod :: term(), Record :: term()}, From :: pid()}.
-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).
-export([route_msg/2]).

-spec start_link(Tab :: ets:tab()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Tab) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab], []).

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([Tab]) ->
    Monitors = maps:from_list([
        {monitor(process, Client), {Sign, Client}}
        || [Sign, Client] <- ets:match(?MODULE, {'$1', {'$2', '_'}})
    ]),
    {ok, #state{tab = Tab, monitors = Monitors}}.

-spec handle_call(Msg, From, State) ->
    reply(Reply, NewState)
    | noreply(NewState)
    | {stop, Reason, Reply, NewState}
    | {stop, Reason, NewState}
when
    Msg :: term(),
    From :: {pid(), Tag},
    Tag :: term(),
    State :: #state{},
    NewState :: #state{},
    Reason :: term(),
    Reply :: term().
handle_call(Msg, From, State) ->
    try
        safe_handle_call(Msg, From, State)
    catch
        E:R:T ->
            ?ERROR("~p, handle call error: ", [?MODULE, {E, R, T}]),
            {reply, {error, ?E_SYSTEM}, State}
    end.

%% 模块内发送错误码可以简化为throw 业务流程上出于明确过程的考虑 采用的是手动发送的方式
safe_handle_call(
    {create, {#auth_ret{platform = Platform, accname = AccName}, #acc_create_c2s{}, _Client}, Args},
    _From,
    State
) ->
    PlatformInt = maps:get(Platform, ?PLATFORM_MAP),
    Acc2PlayerKey = #account2player_keys{accname = AccName, platform = PlatformInt},
    case db_account2player:is_exist(Acc2PlayerKey) of
        true ->
            {reply, {error, ?E_PLAYER_ALREADY_CREATED}, State};
        false ->
            IP = proplists:get_value(ip, Args),
            ID = player_id_server:get_new(),

            {ok, Pid} = player_data_mgr:new(ID),
            player_data:transaction(Pid, fun() ->
                datatable_player:insert(#player_keys{id = ID}, #player_values{
                    sex = 0,
                    name = AccName,
                    last_login_ip = inet:ntoa(IP),
                    head_img = ""
                }),
                datatable_account2player:insert(Acc2PlayerKey, #account2player_values{
                    player_id = ID
                })
            end),
            {reply, {ok, ok}, State}
    end;
safe_handle_call(
    {enter, AuthRet = #auth_ret{accsign = Sign}, {Mod, #acc_enter_c2s{}}, Client},
    _From,
    State
) ->
    %% double check
    case ets:lookup(?MODULE, Sign) of
        [] ->
            try
                %% enter
                #auth_ret{accname = AccName, platform = Platform} = AuthRet,
                PlatformInt = maps:get(Platform, ?PLATFORM_MAP),
                Key = #account2player_keys{accname = AccName, platform = PlatformInt},
                ?IF(
                    db_account2player:is_exist(Key),
                    pass,
                    throw({error, ?E_PLAYER_NO_ROLE})
                ),
                #account2player_values{player_id = ID} = datatable_account2player:get(Key),
                DataRet = player_data_mgr:get_or_new(ID),
                ?IF(?MATCHES({error, _}, DataRet), throw(DataRet), pass),
                ServerRet = player_server_sup:new_player(ID),
                ?IF(?MATCHES({error, _}, ServerRet), throw(ServerRet), pass),
                %% TODO 需要测试无法正常监听关闭的
                {ok, Pid} = ServerRet,
                Ref = monitor(process, Client),
                NewMonitors = maps:put(
                    Ref,
                    {Sign, Client},
                    State#state.monitors
                ),
                ets:insert(?MODULE, {Sign, {Client, Pid}}),
                MsgID = demo_proto_convert:id_mf_convert(Mod, id),
                Msg = #acc_enter_s2c{code = 0},
                {reply, {ok, {send_push, {MsgID, Msg}}}, State#state{
                    monitors = NewMonitors
                }}
            catch
                throw:Err -> {reply, Err, State}
            end;
        [{_, {Client, _Pid}}] ->
            {reply, {error, ?E_PLAYER_ALREADY_ENTER}, State};
        [{_, {Other, Pid}}] ->
            %% todo do replace
            {reply, pass, State}
    end;
safe_handle_call(_Msg, _From, State) ->
    ?INFO("~p, handle unknow msg: ~p~n", [?MODULE, _Msg]),
    {reply, {error, ?E_UNKNOWN}, State}.

-spec handle_cast(Msg, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Msg ::
        term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec route_msg(msg(), Args :: proplists:proplist()) ->
    {ok, Ret :: term()}
    | {error, Reason :: term()}.
route_msg(Msg = {#auth_ret{accsign = Sign}, {Mod, _Record}, From}, Args) ->
    RouteRet =
        case ets:lookup(?MODULE, Sign) of
            [{_, {From, PlayerServer}}] ->
                %% just route
                player_server:route_msg(PlayerServer, Msg, Args);
            _ ->
                route_call(Msg, Args)
        end,
    %% 根据route返回值决定消息回复 错误码之类的
    case RouteRet of
        {ok, noreply} ->
            ok;
        {ok, {SendType, MsgRet}} ->
            proto_util:SendType(From, MsgRet),
            ok;
        {error, Reason} when is_integer(Reason) ->
            {Section, Method} = demo_proto_convert:id_mf_convert(Mod, id),
            ErrMsgID = Section bsl 8 + Method,
            MsgID = demo_proto_convert:id_mf_convert({system, error}, id),
            Msg = #system_error_s2c{msgid = ErrMsgID, code = Reason},
            proto_util:send_push(From, {MsgID, Msg}),
            ok;
        Other ->
            ?ERROR("unexpect route ret: ~p~n", [RouteRet]),
            {error, Other}
    end.

-spec route_call(msg(), Args :: proplists:proplist()) ->
    {ok, Ret :: term()}
    | {error, Reason :: term()}.
route_call(Msg = {_AuthRet, {_Mod, #acc_create_c2s{}}, _From}, Args) ->
    gen_server:call(?MODULE, {create, Msg, Args});
route_call(Msg = {_AuthRet, {_Mod, #acc_enter_c2s{}}, _From}, Args) ->
    gen_server:call(?MODULE, {enter, Msg, Args});
route_call(_Msg, _Args) ->
    {error, need_enter_or_reenter}.
