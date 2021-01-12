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

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/1]).
-export([route_msg/3]).

-export([init/1]).
-export([handle_cast/2, handle_call/3]).

-spec start_link(Client :: pid()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

-spec init(Args :: [term()]) -> {ok, #player{}}.
init([Client]) ->
    {ok, #player{client = Client}}.

route_msg(Pid, Msg, Args) ->
    gen_server:call(Pid, {route_msg, Msg, Args}).

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
handle_call(_Msg, _From, State) ->
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
    {noreply, State}.
