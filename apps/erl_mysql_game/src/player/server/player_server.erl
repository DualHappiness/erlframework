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
    NewPlayer = gen_mod:init_player(#player{client = Client}),
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
    case gen_mod:handle_c2s(Msg, Args, Player) of
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
