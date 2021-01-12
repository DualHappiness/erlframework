-module(module_example).

-ifndef(TEST).
-on_load(init/0).
-endif.

% -behaviour(gen_mod).

-include_lib("erl_logger/include/logger.hrl").
-include_lib("datatable/include/datatable.hrl").
-include_lib("proto_container/include/demo_proto_test.hrl").

init() ->
    code:ensure_loaded(gen_mod),
    ?INFO("mod test loaded"),
    gen_mod:add_module(?MODULE),
    gen_mod:register_handler(test, ?MODULE),

    datatable:start([#datatable_declare{table = test}]),
    loader_template:gen_loader(test),
    ok.
