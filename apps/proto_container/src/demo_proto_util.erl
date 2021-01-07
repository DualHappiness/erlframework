%%%----------------------------------------------
%%%
%%% @doc:  (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_util).

-include("demo_proto.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erl_logger/include/logger.hrl").

-export([de_string/1, en_string/1]).
-export([de_bytes/1, en_bytes/1]).
-export([de_loop/3, en_loop/3, en_loop/4, en_loop_basic/3]).
-export([de_uint8/1, en_uint8/1]).
-export([de_uint16/1, en_uint16/1]).
-export([de_uint32/1, en_uint32/1]).
-export([de_uint64/1, en_uint64/1]).

-export([de_int8/1, en_int8/1]).
-export([de_int16/1, en_int16/1]).
-export([de_int32/1, en_int32/1]).
-export([de_int64/1, en_int64/1]).

%% @doc 对字符串进行解压
de_string(<<>>) ->
    {<<>>, <<>>};
de_string(<<Len:16, Str:Len/bytes, Rest/bytes>>) ->
    {Str, Rest}.

%% @doc 对字符串进行编码
en_string(Str) when is_list(Str) ->
    Len = iolist_size(Str),
    ?assert(Len =< 16#ffff),
    <<Len:16, (list_to_binary(Str))/bytes>>;
en_string(Str) when is_binary(Str) ->
    Len = byte_size(Str),
    ?assert(Len =< 16#7fff),
    <<Len:16, Str/bytes>>.

%% @doc 对bytes字符串进行解压
de_bytes(<<>>) ->
    {<<>>, <<>>};
de_bytes(<<Len:16, Str:Len/bytes, Rest/bytes>>) ->
    {{Len, Str}, Rest}.

%% @doc 对字符串进行编码
en_bytes({Len, Str}) when is_list(Str) ->
    ?assert(Len =< 16#ffff),
    <<Len:16, (list_to_binary(Str))/bytes>>;
en_bytes({Len, Str}) when is_binary(Str) ->
    ?assert(Len =< 16#7fff),
    <<Len:16, Str/bytes>>.

%% @doc 对循环进行解码,参看proto_map:move/1函数
de_loop(<<Loop:?LOOP_SIZE, Rest/bytes>>, Fun, Label) ->
    case de_loop(Rest, Fun, Loop, Loop, []) of
        {ok, N, L, Bin} ->
            {ok, N, L, Bin};
        _ ->
            error({proto, de_loop, Label})
    end;
de_loop(_, _Fun, Label) ->
    error({proto, invalid_loop, Label}).

de_loop(Bin, _Fun, 0, N, Acc) ->
    {ok, N, lists:reverse(Acc), Bin};
de_loop(Bin, Fun, Cur, N, Acc) ->
    case catch Fun(Bin) of
        {'EXIT', _Reason} ->
            {error, loop_fun};
        {Obj, Rest} ->
            Acc2 = [Obj | Acc],
            de_loop(Rest, Fun, Cur-1, N, Acc2)
    end.

%% @doc 将列表打包
en_loop(List, Fun, Label) ->
    en_loop(List, length(List), Fun, Label).

en_loop(List, N, Fun, Label) ->
    try
        Part = lists:map(Fun, List),
        [<<N:?LOOP_SIZE>> | Part]
    catch
        Class:_Reason ->
            ?ERROR("en_loop:~p错误~p:~p", [Label, Class, _Reason]),
            error({proto, en_loop, Label})
    end.

%% @doc 对基础数据类型进行打包
en_loop_basic(List, Type, Label) ->
    try
        en_loop_basic(List, Type)
    catch
        Class:_Reason ->
            ?ERROR("en_loop:~p错误~p:~p", [Label, Class, _Reason]),
            error({proto, en_loop, Label})
    end.

en_loop_basic(List, uint8) ->
    [<<(length(List)):?LOOP_SIZE>>, List];
en_loop_basic(List, uint16) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:16>> || N <- List]];
en_loop_basic(List, uint32) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:32>> || N <- List]];
en_loop_basic(List, uint64) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:64>> || N <- List]];
en_loop_basic(List, bytes) ->
    [<<(length(List)):?LOOP_SIZE>>, [en_bytes(Str) || Str <- List]];
en_loop_basic(List, string) ->
    [<<(length(List)):?LOOP_SIZE>>, [en_string(Str) || Str <- List]];
en_loop_basic(List, int8) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:8/signed>> || N <- List]];
en_loop_basic(List, int16) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:16/signed>> || N <- List]];
en_loop_basic(List, int32) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:32/signed>> || N <- List]];
en_loop_basic(List, int64) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:64/signed>> || N <- List]].

%% @doc uint8解码
de_uint8(<<Obj:8, Rest/binary>>) ->
    {Obj, Rest}.

de_int8(<<Obj:8/signed, Rest/binary>>) ->
    {Obj, Rest}.

en_uint8(Obj) ->
    <<Obj:8>>.

en_int8(Obj) ->
    <<Obj:8/signed>>.

de_uint16(<<Obj:16, Rest/binary>>) ->
    {Obj, Rest}.

de_int16(<<Obj:16/signed, Rest/binary>>) ->
    {Obj, Rest}.

en_uint16(Obj) ->
    <<Obj:16>>.

en_int16(Obj) ->
    <<Obj:16/signed>>.

de_uint32(<<Obj:32, Rest/binary>>)->
    {Obj, Rest}.

de_int32(<<Obj:32/signed, Rest/binary>>)->
    {Obj, Rest}.

en_uint32(Obj) ->
    <<Obj:32>>.

en_int32(Obj) ->
    <<Obj:32/signed>>.

de_uint64(<<Obj:64, Rest/binary>>)->
    {Obj, Rest}.

de_int64(<<Obj:64/signed, Rest/binary>>)->
    {Obj, Rest}.

en_uint64(Obj) ->
    <<Obj:64>>.

en_int64(Obj) ->
    <<Obj:64/signed>>.