%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%     用于把消息转发到指定player
%%% @end
%%% Created : 2021/01/08
%%%-------------------------------------------------------------------
-module(erl_mysql_game_msg_router).

-behaviour(gen_server).

-record(state, {route_map}).

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/0]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-spec start_link() -> {ok, pid()} |
                      {error, {already_started, pid()}} |
                      {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(Args :: [term()]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(Msg, From, State) -> 
    reply(Reply, NewState) | 
    noreply(NewState) | 
    {stop, Reason, Reply, NewState} | 
    {stop, Reason, NewState} 
    when Msg :: term(), 
        From :: {pid(), Tag}, 
        Tag :: term(), 
        State :: #state{}, 
        NewState :: #state{}, 
        Reason :: term(), 
        Reply :: term().
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Msg, State) -> 
    noreply(NewState) | 
    {stop, Reason, NewState}
    when Msg :: term(),
        Reason :: term(),
        State :: #state{},
        NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.