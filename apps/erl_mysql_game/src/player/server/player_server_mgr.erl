%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%%     多读单写来保证性能和数据唯一
%%% @end
%%% Created : 2021/01/08
%%%-------------------------------------------------------------------
-module(player_server_mgr).

-behaviour(gen_server).

-record(state, {tab}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("proto_container/include/demo_proto_acc.hrl").
-include_lib("erl_oauth_lib/include/erl_oauth_lib.hrl").

-type msg() :: {#auth_ret{}, {Mod :: term(), Record :: term()}, From :: pid()}.
-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-export([start_link/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3]).
-export([route_msg/2]).

-spec start_link(Tab :: ets:tab()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Tab) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [Tab]).

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
handle_call(
    {create, {#auth_ret{platform = Platform, accname = AccName}, #acc_create_c2s{}, Client}, Args},
    _From,
    State
) ->
    IP = proplists:get_value(ip, Args),
    ID = player_id_server:get_new(),
    {reply, {ok, ok}, State};
handle_call({enter, #auth_ret{accsign = Sign}, EnterMsg, Client}, _From, State) ->
    %% double check
    Ret =
        case ets:lookup(?MODULE, Sign) of
            [] ->
                %%TODO do enter
                pass;
            [{_, {Client, _Pid}}] ->
                {error, already_enter};
            [{_, {Other, Pid}}] ->
                %% todo do replace
                pass
        end,
    {reply, Ret, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Msg, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Msg ::
        term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec route_msg(msg(), Args :: proplists:proplist()) ->
    {ok, Ret :: term()}
    | {error, Reason :: term()}.
route_msg(Msg = {AuthRet = #auth_ret{accsign = Sign}, {Mod, Record}, From}, Args) ->
    case ets:lookup(?MODULE, Sign) of
        [{_, {From, PlayerServer}}] ->
            %% just route
            player_server:route_msg(PlayerServer, Mod, Record, Args);
        _ ->
            %% TODO do replace or relogin
            route_call(Msg, Args)
    end.

-spec route_call(msg(), Args :: proplists:proplist()) ->
    {ok, Ret :: term()}
    | {error, Reason :: term()}.
route_call(Msg = {_AuthRet, {_Mod, #acc_create_c2s{}}, From}, Args) ->
    gen_server:call(?MODULE, {create, Msg, Args});
route_call(Msg = {_AuthRet, {_Mod, #acc_enter_c2s{}}, From}, Args) ->
    gen_server:call(?MODULE, {enter, Msg, Args});
route_call(_Msg, _Args) ->
    {error, need_enter_or_reenter}.
