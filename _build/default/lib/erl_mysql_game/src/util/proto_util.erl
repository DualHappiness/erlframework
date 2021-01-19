-module(proto_util).

-export([send_push/2, send_merge/2]).

send_push(Client, Data) ->
    Client ! {send_push, Data}.

send_merge(Client, Data) ->
    Client ! {send_merge, Data}.
