%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% description
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data_mgr).

-behaviour(gen_server).

-record(state, {tab}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erl_logger/include/logger.hrl").

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-type id() :: player:id().

-compile({no_auto_import, [{get, 1}]}).

-export([start_link/1]).
-export([new/1, get/1, get_or_new/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-spec start_link(Tab :: ets:tab()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Tab) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab], []).

-spec new(ID :: id()) -> {ok, pid()} | {error, Reason :: term()}.
new(ID) ->
    ?assertMatch(undefined, get(ID)),
    gen_server:call(?MODULE, {new, ID}).

-spec get(ID :: id()) -> undefined | pid().
get(ID) ->
    case ets:lookup(?MODULE, ID) of
        [] ->
            undefined;
        [{_, V}] ->
            V
    end.

-spec get_or_new(ID :: id()) -> {ok, pid()} | {error, Reason :: term()}.
get_or_new(ID) ->
    case get(ID) of
        undefined ->
            new(ID);
        Pid ->
            {ok, Pid}
    end.

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([Tab]) ->
    {ok, #state{tab = Tab}}.

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
            {reply, {error, system}, State}
    end.

safe_handle_call({new, ID}, _From, State) ->
    case get(ID) of
        Pid when is_pid(Pid) ->
            {reply, {ok, Pid}, State};
        undefined ->
            case player_data_sup:new_player_data(ID) of
                {error, _} = Err ->
                    {reply, Err, State};
                {ok, Pid} ->
                    ets:insert(State#state.tab, {ID, Pid}),
                    {reply, {ok, Pid}, State}
            end
    end;
safe_handle_call(_Msg, _From, State) ->
    ?INFO("~p, unknow call msg: ~p~n, from :~p~n", [?MODULE, _Msg, _From]),
    {reply, unknow, State}.

-spec handle_cast(Msg, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Msg ::
        term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.
