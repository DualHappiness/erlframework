%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% description
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data).

-behaviour(gen_server).

-record(state, {last_time = 0}).

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-type id() :: player:id().

-include_lib("desc_container/include/datadesc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([transaction/2, read_transaction/2]).

-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-spec start_link(ID :: id()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(ID) ->
    gen_server:start_link(?MODULE, [ID], []).

-spec transaction(PidOrID, F) -> {ok, Ret} | {error, Reason} when
    PidOrID :: pid() | player:id(), F :: fun(() -> Ret), Ret :: term(), Reason :: term().
transaction(Pid, F) when is_pid(Pid) ->
    gen_server:call(Pid, {transaction, F});
transaction(ID, F) ->
    case player_data_mgr:get(ID) of
        undefined ->
            {error, incorrect_id};
        Pid ->
            transaction(Pid, F)
    end.

%% !WARN 仅是为了方便跨节点同时取多个数据，减少rpc调用，禁止用来写数据
-spec read_transaction(IDorPid, F) -> {ok, Ret} | {error, Reason} when
    IDorPid :: id() | pid(),
    F :: fun(() -> Ret),
    Ret :: term(),
    Reason :: term().
read_transaction(Pid, F) when is_pid(Pid) ->
    gen_server:cast(Pid, read_transaction),
    %% TODO 增加开发时查找F中有没有写入的功能，可以考虑对所以相关数据加锁
    {ok, F()};
read_transaction(ID, F) ->
    case player_data_mgr:get(ID) of
        undefined ->
            {error, incorrect_id};
        Pid ->
            read_transaction(Pid, F)
    end.

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([ID]) ->
    %% init player data
    db_player:is_exist(#player_keys{id = ID}),
    %% init player module data
    gen_mod:init_data(ID),
    {ok, #state{last_time = game_util:now_seconds()}}.

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
handle_call({transaction, F}, _From, State) ->
    Ret =
        case catch F() of
            {error, _} = Err -> Err;
            R -> {ok, R}
        end,
    {reply, Ret, State#state{last_time = game_util:now_seconds()}};
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
handle_cast(read_transaction, State) ->
    {noreply, State#state{last_time = game_util:now_seconds()}};
handle_cast(_Msg, State) ->
    {noreply, State}.
