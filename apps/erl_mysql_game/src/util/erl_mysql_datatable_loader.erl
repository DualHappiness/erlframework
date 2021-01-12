-module(erl_mysql_datatable_loader).

-export([gen_loader/1]).

gen_loader(Mod) ->
    Template =
        "-module(~s).\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "-include_lib(\"desc_container/include/datadesc.hrl\").\n"
        "-include_lib(\"erl_logger/include/logger.hrl\").\n"
        "-include_lib(\"pmod/include/pmod.hrl\").\n"
        "\n"
        "-export([load/1, is_exist/1]).\n"
        "\n"
        "-spec load(any()) -> ok.\n"
        "load(Key) ->\n"
        "    Db = dataloggersql_helper:get_db(),\n"
        "    case Db:select(\"~p\", fields_str(#~p_values{}), where(Key)) of\n"
        "        {selected, _, []} ->\n"
        "            pass;\n"
        "        {selected, _, [Data]} ->\n"
        "            datatable_~p:raw_insert(Key, list_to_tuple([~p_values | Data]));\n"
        "        Err = {error, Reason} ->\n"
        "            ?ERROR(\"load failed~~nloader: ~~p~~nkey: ~~p~~nreason: ~~p~~n\", [?MODULE, Key, Reason]),\n"
        "            throw(Err)\n"
        "    end,\n"
        "    ok."
        "\n"
        "-spec is_exist(any()) -> boolean().\n"
        "%% ! 每一条数据使用前一定要确保检查过一次存在\n"
        "is_exist(Key) ->\n"
        "    datatable_loader:load(?MODULE, Key),\n"
        "   not ?MATCHES(undefined, datatable_account:get(Key)).\n",

    FileNameBase = io_lib:format("erl_mysql_datatable_db_~p", [Mod]),
    CodeStr = io_lib:format(unicode:characters_to_binary(Template), [
        FileNameBase
        | lists:duplicate(4, Mod)
    ]),
    erl_dynamic:compile_and_load(FileNameBase ++ ".erl", CodeStr),
    ok.
