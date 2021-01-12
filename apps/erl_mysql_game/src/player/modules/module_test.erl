-module(module_test).

-on_load(init/0).

-include_lib("erl_logger/include/logger.hrl").

init() ->
    code:ensure_loaded(gen_mod),
    ?INFO("mod test loaded"),
    gen_mod:add_module(?MODULE),
    ok.
