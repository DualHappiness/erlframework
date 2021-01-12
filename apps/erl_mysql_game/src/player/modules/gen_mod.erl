%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% 
%%% @end
%%% Created : 2021/01/12
%%%-------------------------------------------------------------------
%% PLAYER 代表的是session的数据
%% 严格区分本地和远程调用
%% 远程调用没有player
%% 对一个player数据的修改 必须经过相关player_data

-module(gen_mod).

-behaviour(gen_server).

-record(state, {}).

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/0]).
-export([get_all_module/0]).
-export([add_module/1, remove_module/1]).
-export([register_handler/2, unregister_handler/1, find_handler/1]).

-export([init/1]).
-export([handle_cast/2, handle_call/3]).

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
    ets:insert(?MODULE, {mod, Mod}).

remove_module(Mod) ->
    ets:delete(?MODULE, {mod, Mod}).

%% TODO 单消息多处理
register_handler(Section, Mod) ->
    ets:insert(?MODULE, {{msg, Section}, Mod}).

unregister_handler(Section) ->
    ets:delete(?MODULE, {msg, Section}).

find_handler({Section, _Part}) ->
    case ets:lookup(?MODULE, {msg, Section}) of
        [] -> {error, no_handler};
        [{_, Mod}] -> {ok, Mod}
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
