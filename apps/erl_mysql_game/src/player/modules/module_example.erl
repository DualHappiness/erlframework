-module(module_example).

-behaviour(gen_mod).

-ifndef(TEST).
-on_load(init/0).
-endif.

-include("player.hrl").

-include_lib("erl_logger/include/logger.hrl").
-include_lib("datatable/include/datatable.hrl").
-include_lib("proto_container/include/demo_proto_test.hrl").
-include_lib("proto_container/include/demo_proto_error_code.hrl").

-type player() :: player:player().

-export([init/0]).
-export([handle_c2s/3]).

-spec init() -> ok.
init() ->
    code:ensure_loaded(gen_mod),
    ?INFO("mod test loaded"),
    gen_mod:add_module(?MODULE),
    gen_mod:register_handler(test, ?MODULE),

    datatable:start([#datatable_declare{table = test}]),
    loader_template:gen_loader(test),
    ok.

-spec handle_c2s(term(), term(), player()) -> gen_mod:mod_reply().
handle_c2s({MsgID, #test_part1_c2s{value = V}}, _Args, Player) ->
    {ok, {send_push, {MsgID, #test_part1_s2c{value = V + 1}}}, Player};
handle_c2s(_Msg, _Args, _Player) ->
    ?ERROR("~p receive unknow c2c msg: ~p~n", [?MODULE, {_Msg, _Args}]),
    {error, ?E_SYSTEM}.
