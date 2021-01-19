%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%     多读单写来保证性能和数据唯一
%%% @end
%%% Created : 2021/01/08
%%%-------------------------------------------------------------------
-module(player_server_mgr).

-behaviour(gen_server).

-record(state, {monitors = #{}, pid2ref = #{}}).

-include_lib("erl_logger/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("proto_container/include/demo_proto_acc.hrl").
-include_lib("proto_container/include/demo_proto_system.hrl").
-include_lib("proto_container/include/demo_proto_error_code.hrl").
-include_lib("desc_container/include/datadesc.hrl").
-include_lib("erl_oauth_lib/include/erl_oauth_lib.hrl").
-include_lib("erl_mysql_common/include/project.hrl").

-type msg() :: {#auth_ret{}, {Mod :: term(), Record :: term()}, From :: pid()}.
-type msg_ret() ::
    {ok, noreply}
    | {ok, {SendType :: send_push | send_merge, Ret :: term()}}
    | {error, Reason :: term()}.

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export_type([msg/0, msg_ret/0]).

-define(PLAYER_ID2PID_TAB, player_server_id2pid).

-export([start_link/1]).
-export([new_acc_tab/0]).
-export([get_pid/1, route_msg/2]).

-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).

-spec start_link(Tab :: ets:tab()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Tab) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab], []).

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([_Tab]) ->
    new_id2player_tab(),
    State = rebuild_id_map(#state{}),
    {ok, State}.

-spec new_acc_tab() -> ets:tab().
new_acc_tab() ->
    ets:new(player_server_mgr, [public, set, named_table, {read_concurrency, true}]).

-spec get_pid(player:id()) -> undefined | pid().
get_pid(ID) ->
    case ets:lookup(?PLAYER_ID2PID_TAB, ID) of
        [] -> undefined;
        [{_, Pid}] -> Pid
    end.

-spec get_acc_pid(AccID :: term()) -> undefined | {Client :: pid(), PlayerServer :: pid()}.
get_acc_pid(AccID) ->
    case ets:lookup(?MODULE, AccID) of
        [] -> undefined;
        [{_, _ID, V}] -> V
    end.

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
            ?ERROR("~p, handle call error: ~p~n ", [?MODULE, {E, R, T}]),
            {reply, {error, ?E_SYSTEM}, State}
    end.

%% 模块内发送错误码可以简化为throw 业务流程上出于明确过程的考虑 采用的是手动发送的方式
safe_handle_call(
    {create, {#auth_ret{platform = Platform, accname = AccName}, {Mod, #acc_create_c2s{}}, _Client},
        Args},
    _From,
    State
) ->
    PlatformInt = maps:get(Platform, ?PLATFORM_MAP),
    Acc2PlayerKey = #account2player_keys{accname = AccName, platform = PlatformInt},
    case db_account2player:is_exist(Acc2PlayerKey) of
        true ->
            {reply, {error, ?E_PLAYER_ALREADY_CREATED}, State};
        false ->
            ?DEBUG("args : ~p~n", [Args]),
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
            MsgID = demo_proto_convert:id_mf_convert(Mod, id),
            Msg = #acc_create_s2c{id = ID},
            {reply, {ok, {send_push, {MsgID, Msg}}}, State}
    end;
safe_handle_call(
    {enter,
        Msg = {#auth_ret{accname = AccName, platform = Platform}, {_Mod, #acc_enter_c2s{}}, Client},
        _Args},
    _From,
    State
) ->
    %% double check
    case get_acc_pid({AccName, Platform}) of
        undefined ->
            do_enter(Msg, State);
        {Client, _Pid} ->
            {reply, {error, ?E_PLAYER_ALREADY_ENTER}, State};
        {_Other, Pid} ->
            Ref = maps:get(Pid, State#state.pid2ref),
            NewState = remove_player(Ref, State),
            player_server:stop(Pid, ?E_PLAYER_KICK_BY_OTHER),
            do_enter(Msg, NewState)
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

-spec handle_info(Info, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Info :: term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_info(
    {'DOWN', Ref, process, _Pid, _Reason},
    State
) ->
    NewState = remove_player(Ref, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    ?ERROR("~p receive unknow info: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

-spec route_msg(msg(), Args :: proplists:proplist()) ->
    {ok, Ret :: term()}
    | {error, Reason :: term()}.
route_msg(Msg = {#auth_ret{accname = AccName, platform = Platform}, {Mod, _Record}, From}, Args) ->
    RouteRet =
        case get_acc_pid({AccName, Platform}) of
            {From, PlayerServer} ->
                %% just route
                player_server:route_msg(PlayerServer, Msg, Args);
            _ ->
                route_call(Msg, Args)
        end,
    %% 根据route返回值决定消息回复 错误码之类的
    case RouteRet of
        {ok, noreply} ->
            {ok, ok};
        {ok, {SendType, MsgRet}} ->
            proto_util:SendType(From, MsgRet),
            {ok, ok};
        {error, Reason} when is_integer(Reason) ->
            {Section, Method} = demo_proto_convert:id_mf_convert(Mod, id),
            ErrMsgID = Section bsl 8 + Method,
            ID = demo_proto_convert:id_mf_convert({system, error}, id),
            Body = #system_error_s2c{msgid = ErrMsgID, code = Reason},
            proto_util:send_push(From, {ID, Body}),
            {ok, ok};
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
    {error, ?E_PLAYER_NEED_ENTER}.

-spec do_enter(msg(), #state{}) -> reply(term(), #state{}).
do_enter(
    {#auth_ret{accname = AccName, platform = Platform}, {Mod, #acc_enter_c2s{}}, Client},
    State
) ->
    try
        AccID = {AccName, Platform},
        %% enter
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
        ServerRet = player_server_sup:new_player(ID, Client),
        ?IF(?MATCHES({error, _}, ServerRet), throw(ServerRet), pass),

        {ok, Pid} = ServerRet,
        NewState = add_player(AccID, ID, Client, Pid, State),

        MsgID = demo_proto_convert:id_mf_convert(Mod, id),
        Msg = #acc_enter_s2c{code = 0},
        {reply, {ok, {send_push, {MsgID, Msg}}}, NewState}
    catch
        throw:Err -> {reply, Err, State}
    end.

%%% data 相关
-spec new_id2player_tab() -> ets:tab().
new_id2player_tab() ->
    ets:new(?PLAYER_ID2PID_TAB, [protected, set, named_table, {read_concurrency, true}]).

-spec mon_player(term(), player:id(), pid(), {map(), map()}) -> {map(), map()}.
mon_player(AccID, ID, Pid, {Monitors, Pid2Ref}) ->
    Ref = monitor(process, Pid),
    {Monitors#{Ref => {AccID, ID, Pid}}, Pid2Ref#{Pid => Ref}}.

-spec rebuild_id_map(State :: #state{}) -> State :: #state{}.
rebuild_id_map(State) ->
    {Monitors, Pid2Ref} = rebuild_loop(ets:first(?MODULE), {#{}, #{}}),
    State#state{monitors = Monitors, pid2ref = Pid2Ref}.

-spec rebuild_loop(term() | '$end_of_table', {map(), map()}) -> {map(), map()}.
rebuild_loop('$end_of_table', Ret) ->
    Ret;
rebuild_loop(AccID, Ret) ->
    [{AccID, ID, {_Client, Pid}}] = ets:lookup(?MODULE, AccID),
    ets:insert(?PLAYER_ID2PID_TAB, {ID, Pid}),
    rebuild_loop(ets:next(?MODULE, AccID), mon_player(AccID, ID, Pid, Ret)).

-spec add_player(
    AccID :: term(),
    player:id(),
    Client :: pid(),
    Player :: pid(),
    State :: #state{}
) -> NewState :: #state{}.
add_player(AccID, ID, Client, Pid, State) ->
    {NewMonitors, NewPid2Ref} = mon_player(
        AccID,
        ID,
        Pid,
        {State#state.monitors, State#state.pid2ref}
    ),
    %% add related data
    ets:insert(?MODULE, {AccID, ID, {Client, Pid}}),
    ets:insert(?PLAYER_ID2PID_TAB, {ID, Pid}),
    State#state{monitors = NewMonitors, pid2ref = NewPid2Ref}.

remove_player(Ref, State = #state{monitors = Monitors, pid2ref = Pid2Ref}) ->
    case maps:is_key(Ref, Monitors) of
        false ->
            State;
        true ->
            demonitor(Ref, [flush]),
            #{Ref := {AccID, ID, Pid}} = Monitors,
            ets:delete(?MODULE, AccID),
            ets:delete(?PLAYER_ID2PID_TAB, ID),
            NewMonitors = maps:remove(Ref, Monitors),
            NewPid2Ref = maps:remove(Pid, Pid2Ref),
            State#state{monitors = NewMonitors, pid2ref = NewPid2Ref}
    end.
