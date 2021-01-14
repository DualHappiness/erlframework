%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%
%%% @end
%%% Created : 2021/01/11
%%%-------------------------------------------------------------------
-module(player_id_server).

-behaviour(gen_server).

-include_lib("erl_logger/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("pmod/include/pmod.hrl").

-record(state, {config_tab}).

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/0]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).
-export([get_new/0]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_new() -> non_neg_integer().
get_new() ->
    gen_server:call(?MODULE, get_new).

%% =====================================================
-spec init(Args :: [term()]) -> {ok, #state{}}.
init([]) ->
    ConfigPath = env_util:get_config_path(),
    {ok, Tab} = dets:open_file(
        ?MODULE,
        [{file, ConfigPath}, {auto_save, 60 * 1000}, {repair, true}]
    ),
    init_player_id(Tab),
    {ok, #state{config_tab = Tab}}.

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
            ?ERROR("get new from ~p error. reason is : ~p~n.", [?MODULE, {E, R, T}]),
            {reply, {error, system_error}, State}
    end.

safe_handle_call(get_new, _From, State = #state{config_tab = Tab}) ->
    ID = dets:update_counter(Tab, ?MODULE, 1),
    {reply, ID, State};
safe_handle_call(_Msg, _From, State) ->
    ?INFO("unknow msg from: ~p, msg is: ~p~n", [_From, _Msg]),
    {reply, unknow, State}.

-spec handle_cast(Msg, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Msg ::
        term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

init_player_id(Tab) ->
    case
        not ?MATCHES(force_init, env_util:get_config_strategy()) andalso
            dets:member(Tab, ?MODULE)
    of
        true ->
            pass;
        false ->
            ServerID = lists:min(env_util:get_server_ids()),
            ID = max(get_max_id(), ServerID * 100_000_000 + env_util:get_player_id_start()),
            ?INFO("init id: ~p~n", [{ServerID, ID}]),
            dets:insert(Tab, {?MODULE, ID})
    end.

%% HACK 安排个合适的地方 毕竟现在有点跟表结构绑死了 而且通过的是约定来确保表和字段的关系
get_max_id() ->
    Db = dataloggersql_helper:get_db(),
    case Db:select("player", ["max(id)"], []) of
        {error, _Reason} ->
            ?ERROR("查询玩家最大id失败: ~p~n", [_Reason]),
            throw({error, db_error});
        {selected, _Fields, [[undefined]]} ->
            0;
        {selected, _Fields, [[MaxId]]} ->
            MaxId
    end.
