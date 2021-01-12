-module(module_example).

-ifndef(TEST).
-on_load(init/0).
-endif.

% -behaviour(gen_mod).

-include_lib("erl_logger/include/logger.hrl").

init() ->
    code:ensure_loaded(gen_mod),
    ?INFO("mod test loaded"),
    gen_mod:add_module(?MODULE),
    ok.
