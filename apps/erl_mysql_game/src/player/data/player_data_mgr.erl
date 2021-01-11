%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% description
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data_mgr).

-behaviour(gen_server).

-record(state, {}).

-include_lib("erl_logger/include/logger.hrl").

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/0]).
-export([new/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-spec start_link() ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec new(ID :: term()) -> {ok, pid()} | {error, Reason :: term()}.
new(ID) ->
    gen_server:call(?MODULE, {new, ID}).

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

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
