%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%
%%% @end
%%% Created : 2021/01/12
%%%-------------------------------------------------------------------
%% ! gen mod 里需要保证所有的handle 不能抛错，保证测试不全导致线上不稳定
%% ! 各类init里是可以抛错的 做到错误早发现
%% PLAYER 代表的是session的数据
%% 严格区分本地和远程调用
%% 远程调用没有player
%% 对一个player数据的修改 必须经过相关player_data

-module(gen_mod).

-behaviour(gen_server).

-include("player.hrl").

-include_lib("proto_container/include/demo_proto_error_code.hrl").
-include_lib("erl_logger/include/logger.hrl").

-record(state, {}).

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/0]).
-export([get_all_module/0]).
-export([add_module/1, remove_module/1]).
-export([register_handler/2, unregister_handler/1, find_handler/1]).
-export([init_data/1, terminate_data/1]).
-export([init_player/1, terminate_player/1]).
-export([handle_c2s/3, handle_s2s_call/3, handle_s2s_cast/3]).

-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-type id() :: player:id().
-type player() :: player:player().

-callback init() -> ok.
-callback init_data(ID :: id()) -> ok.
-callback terminate_data(ID :: id()) -> ok.
-callback init_player(player()) -> player().
-callback terminate_player(player()) -> player().

-optional_callbacks([init/0, init_data/1, terminate_data/1, init_player/1, terminate_player/1]).

-type mod_ret() :: {ok, player()} | {error, Reason :: term()}.
-type mod_reply() :: {ok, Reply :: term(), player()} | {error, Reason :: term()}.

-export_type([mod_ret/0, mod_reply/0]).

-callback handle_c2s({MsgID :: term(), Record :: term()}, Args :: proplists:proplist(), player()) ->
    mod_reply().

-callback handle_s2s_call(term(), player()) -> mod_reply().
-callback handle_s2s_cast(term(), player()) -> mod_ret().

%% TODO remote call/cast
-callback handle_remote_call(term()) -> {ok, Reply :: term()} | {error, Reason :: term()}.
-callback handle_remote_cast(term()) -> ok | {error, Reason :: term()}.

-optional_callbacks([handle_remote_call/1, handle_remote_cast/1]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([]) ->
    ets:new(?MODULE, [public, set, named_table, {read_concurrency, true}]),
    %% 利用on_load来实现init 这样热更的时候就会更简单
    [Mod:module_info() || Mod <- get_all_module()],
    {ok, #state{}}.

get_all_module() ->
    %% mod 有系统开头的文件 虽然不会有啥影响 但还是区分开比较好
    [list_to_atom(Mod) || {Mod = "module" ++ _, _Path, _Loaded} <- code:all_available()].

add_module(Mod) ->
    ets:insert(?MODULE, {{mod, Mod}, Mod}).

remove_module(Mod) ->
    ets:delete(?MODULE, {mod, Mod}).

get_added_module() ->
    [Mod || [Mod] <- ets:match(?MODULE, {{mod, '$1'}, '_'})].

%% TODO 单消息多处理
register_handler(Section, Mod) ->
    ets:insert(?MODULE, {{msg, Section}, Mod}).

unregister_handler(Section) ->
    ets:delete(?MODULE, {msg, Section}).

find_handler({Section, _Part}) ->
    case ets:lookup(?MODULE, {msg, Section}) of
        [] -> undefined;
        [{_, Mod}] -> Mod
    end.

init_data(ID) ->
    [
        case erlang:function_exported(Mod, init_data, 1) of
            false -> pass;
            true -> Mod:init_data(ID)
        end
        || Mod <- get_added_module()
    ],
    ok.

%% 单服应该最多也就百万的数据所以没考虑过卸载
terminate_data(ID) ->
    [
        case erlang:function_exported(Mod, terminate_data, 1) of
            false -> pass;
            true -> Mod:terminate_data(ID)
        end
        || Mod <- get_added_module()
    ],
    ok.

init_player(Player) ->
    lists:foldl(
        fun(Mod, LocalPlayer) ->
            case erlang:function_exported(Mod, init_player, 1) of
                false -> LocalPlayer;
                true -> Mod:init_player(LocalPlayer)
            end
        end,
        Player,
        get_added_module()
    ).

terminate_player(Player) ->
    lists:foldl(
        fun(Mod, LocalPlayer) ->
            case erlang:function_exported(Mod, terminate_player, 1) of
                false -> LocalPlayer;
                true -> Mod:terminate_player(LocalPlayer)
            end
        end,
        Player,
        get_added_module()
    ).

%% HACK 暂时args之类的都没用上 只是为了占位
-spec handle_c2s(player_server_mgr:msg(), term(), player()) -> mod_reply().
handle_c2s({_AuthRet, {Mod, Record}, _From}, Args, Player) ->
    case find_handler(Mod) of
        undefined ->
            {error, ?E_SYSTEM};
        HandleMod ->
            MsgID = demo_proto_convert:id_mf_convert(Mod, id),
            try
                HandleMod:handle_c2s({MsgID, Record}, Args, Player)
            catch
                thorw:Err ->
                    Err;
                E:R:T ->
                    ?ERROR("~p handle c2s ~p error: ~p~n", [
                        HandleMod,
                        {Mod, Record, Args},
                        {E, R, T}
                    ]),
                    {error, {E, R, T}}
            end
    end.

-spec handle_s2s_call(Section :: term(), Args :: term(), player()) -> mod_reply().
handle_s2s_call(Section, Args, Player) ->
    case find_handler({Section, undefined}) of
        undefined ->
            {error, ?E_SYSTEM};
        Mod ->
            try
                Mod:handle_s2s_call(Args, Player)
            catch
                throw:Err ->
                    Err;
                E:R:T ->
                    ?ERROR("~p handle s2s call ~p error: ~p~n", [Mod, Args, {E, R, T}]),
                    {error, {E, R, T}}
            end
    end.

-spec handle_s2s_cast(Section :: term(), Args :: term(), player()) -> mod_ret().
handle_s2s_cast(Section, Args, Player) ->
    case find_handler({Section, undefined}) of
        undefined ->
            {error, ?E_SYSTEM};
        Mod ->
            try
                Mod:handle_s2s_cast(Args, Player)
            catch
                throw:Err ->
                    Err;
                E:R:T ->
                    ?ERROR("~p handle s2s cast ~p error: ~p~n", [Mod, Args, {E, R, T}]),
                    {error, {E, R, T}}
            end
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
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Msg, State) ->
    noreply(NewState)
    | {stop, Reason, NewState}
when
    Msg :: term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.
