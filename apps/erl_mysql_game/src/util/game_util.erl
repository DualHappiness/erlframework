-module(game_util).

-compile(export_all).
-compile(nowarn_export_all).

now_seconds() ->
    erlang:system_time(seconds).

mod_to_id(Mod) ->
    {S, P} = demo_proto_convert:id_mf_convert(Mod, id),
    S bsl 8 + P.
