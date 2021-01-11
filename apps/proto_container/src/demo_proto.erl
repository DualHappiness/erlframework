-module(demo_proto).

-export([en_packet/2, de_packet/1]).

de_packet(Bin) ->
    demo_proto_c2s:de_packet(Bin).

en_packet(ID, Msg) -> demo_proto_s2c:en_packet(ID, Msg).
