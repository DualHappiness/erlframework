%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%
%%% @end
%%% Created : 2021/01/08
%%%-------------------------------------------------------------------
-module(player_server).

-behaviour(gen_server).

-include("player.hrl").

-include_lib("erl_logger/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("proto_container/include/demo_proto_error_code.hrl").
-include_lib("proto_container/include/demo_proto_system.hrl").

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/2]).
-export([route_msg/3]).

-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2]).

-spec start_link(ID :: player:id(), Client :: pid()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(ID, Client) ->
    gen_server:start_link(?MODULE, [ID, Client], []).

-spec init(Args :: [term()]) -> {ok, #player{}}.
init([ID, Client]) ->
    process_flag(trap_exit, true),
    link(Client),
    NewPlayer = gen_mod:init_player(#player{client = Client, id = ID}),
    {ok, NewPlayer}.

-spec route_msg(pid(), player_server_mgr:msg(), term()) -> player_server_mgr:msg_ret().
route_msg(Pid, Msg, Args) ->
    gen_server:call(Pid, {route_msg, Msg, Args}).

% -spec s2s_call(IDorPid :: non_neg_integer() | pid(), Section :: term(), Args :: term()) -> term().
% s2s_call(ID) ->

-spec handle_call(Msg, From, State) ->
    reply(Reply, NewState)
    | noreply(NewState)
    | {stop, Reason, Reply, NewState}
    | {stop, Reason, NewState}
when
    Msg :: term(),
    From :: {pid(), Tag},
    Tag :: term(),
    State :: #player{},
    NewState :: #player{},
    Reason :: term(),
    Reply :: term().
%% gen mod 里保证调用肯定不会抛错
handle_call({route_msg, Msg, Args}, _From, Player) ->
    case gen_mod:handle_c2s(Msg, Args, cache_check(Player)) of
        {error, _} = Err -> {reply, Err, Player};
        {ok, Reply, NewPlayer} -> {reply, Reply, NewPlayer}
    end;
handle_call(_Msg, _From, State) ->
    ?ERROR("~p receive unknow call: ~p~n", [?MODULE, _Msg]),
    {reply, ok, State}.

-spec handle_cast(Msg, State) ->
    noreply(NewState)
    | {stop, Reason, NewState}
when
    Msg :: term(),
    Reason :: term(),
    State :: #player{},
    NewState :: #player{}.
handle_cast(_Msg, State) ->
    ?ERROR("~p receive unknow cast: ~p~n", [?MODULE, _Msg]),
    {noreply, State}.

-spec handle_info(Info, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Info :: term(),
    State :: #player{},
    Reason :: term(),
    NewState :: #player{}.
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, Player = #player{data_ref = _Ref}) ->
    {noreply, Player#player{data_pid = undefined}};
handle_info({'EXIT', Pid, relogin}, Player) when Pid =:= self() ->
    {stop, relogin, Player};
handle_info({'EXIT', _Client, _Reason}, Player = #player{client = _Client}) ->
    ?INFO("client ~p exit~n", [_Client]),
    {stop, client_exit, Player};
handle_info(_Info, State) ->
    ?ERROR("~p handle unknow info: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

-spec terminate(Reason :: term(), #player{}) -> term().
terminate(Reason, Player) ->
    gen_mod:terminate_player(cache_check(Player)),
    case Reason of
        client_exit ->
            pass;
        %% TODO 非意外情况特殊处理
        _ when is_integer(Reason) ->
            MsgID = demo_proto_convert:id_mf_convert({system, error}, id),
            Msg = #system_error_s2c{msgid = 0, code = Reason},
            exit({exit_msg, {MsgID, Msg}});
        _ ->
            %% HACK 利用'EXIT'消息特殊处理的逻辑  期待更好的写法
            MsgID = demo_proto_convert:id_mf_convert({system, error}, id),
            Msg = #system_error_s2c{msgid = 0, code = ?E_PLAYER_TERMINATE},
            %% TODO 如果各种这种方式影响到了各种报警系统 则换成直接发消息
            exit({exit_msg, {MsgID, Msg}})
    end,
    ok.

cache_check(Player = #player{data_pid = undefined}) ->
    Player;
cache_check(Player = #player{id = ID}) ->
    Pid = player_data_mgr:get(ID),
    Ref = monitor(process, Pid),
    Player#player{data_pid = Pid, data_ref = Ref}.
