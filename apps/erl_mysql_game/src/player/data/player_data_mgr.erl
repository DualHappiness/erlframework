%%%-------------------------------------------------------------------
%%% @author dualwu
%%% @doc
%%% description
%%% @end
%%% Created : 2021/01/09
%%%-------------------------------------------------------------------
-module(player_data_mgr).

-behaviour(gen_server).

-record(state, {monitors :: map()}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erl_logger/include/logger.hrl").

-type hibernate_term() :: timeout() | hibernate.
-type reply(Term, State) :: {reply, Term, State} | {reply, Term, State, hibernate_term()}.
-type noreply(State) :: {noreply, State} | {noreply, State, hibernate_term()}.

-type id() :: player:id().

-compile({no_auto_import, [{get, 1}]}).

-export([start_link/1]).
-export([new_data_tab/0]).
-export([new/1, get/1, get_or_new/1]).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).

-spec start_link(Tab :: ets:tab()) ->
    {ok, pid()}
    | {error, {already_started, pid()}}
    | {error, Reason :: any()}.
start_link(Tab) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab], []).

-spec new_data_tab() -> ets:tab().
new_data_tab() ->
    ets:new(player_data_mgr, [set, public, named_table, {read_concurrency, true}]).

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
init([_Tab]) ->
    State = rebuild_monitor(#state{}),
    {ok, State}.

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
            case player_data_sup:new(ID) of
                {error, _} = Err ->
                    {reply, Err, State};
                {ok, Pid} ->
                    NewState = add_data(ID, Pid, State),
                    {reply, {ok, Pid}, NewState}
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

-spec handle_info(Info, State) -> noreply(NewState) | {stop, Reason, NewState} when
    Info :: term(),
    Reason :: term(),
    State :: #state{},
    NewState :: #state{}.
handle_info({'DOWN', Ref, process, Pid, Reason}, State) when
    Reason =:= normal; Reason =:= shutdown
->
    NewState = remove_data(Ref, Pid, State),
    {noreply, NewState};
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    ?ERROR("player data down abnormal. Reason is :~p~n", [_Reason]),
    case maps:is_key(Ref, State#state.monitors) of
        false ->
            {noreply, State};
        true ->
            #{Ref := {ID, Pid}} = State#state.monitors,
            State1 = remove_data(Ref, Pid, State),
            %% ! 确保一定会成功
            {ok, NewPid} = player_data_sup:new(ID),
            State2 = add_data(ID, NewPid, State1),
            {noreply, State2}
    end;
handle_info(_Info, State) ->
    ?ERROR("~p receive unknow info: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

%%==============================================================================
%% data api
%%==============================================================================
-spec mon_data(player:id(), pid(), Monitors :: map()) -> NewMonitors :: map().
mon_data(ID, Pid, Monitors) ->
    Ref = monitor(process, Pid),
    Monitors#{Ref => {ID, Pid}}.

-spec rebuild_monitor(State :: #state{}) -> NewState :: #state{}.
rebuild_monitor(State) ->
    Monitors = rebuild_loop(ets:first(?MODULE), #{}),
    State#state{monitors = Monitors}.

-spec rebuild_loop(Key :: term() | '$end_of_table', Monitors :: map()) -> NewMonitors :: map().
rebuild_loop('$end_of_table', Monitors) ->
    Monitors;
rebuild_loop(ID, Monitors) ->
    [{ID, Pid}] = ets:lookup(?MODULE, ID),
    rebuild_loop(ets:next(?MODULE, ID), mon_data(ID, Pid, Monitors)).

-spec add_data(ID :: player:id(), Pid :: pid(), State :: #state{}) -> NewState :: #state{}.
add_data(ID, Pid, State) ->
    ets:insert(?MODULE, {ID, Pid}),
    NewMonitors = mon_data(ID, Pid, State#state.monitors),
    State#state{monitors = NewMonitors}.

-spec remove_data(reference(), pid(), State :: #state{}) -> NewState :: #state{}.
remove_data(Ref, Pid, State = #state{monitors = Monitors}) ->
    case maps:is_key(Ref, Monitors) of
        false ->
            State;
        true ->
            #{Ref := {ID, Pid}} = Monitors,
            demonitor(Ref, [flush]),
            ets:delete(?MODULE, ID),
            NewMonitors = maps:remove(Ref, Monitors),
            State#state{monitors = NewMonitors}
    end.
