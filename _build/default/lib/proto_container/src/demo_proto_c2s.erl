%%%----------------------------------------------
%%%
%%% @doc: 
%%%   所有proto_xxx模块中
%%%  de_xxx 函数参数为binary返回值为{ok, record}
%%%  en_xxx 函数参数为term返回值为iodata
%%% @end
%%%
%%%----------------------------------------------------------------------
%%% (自动生成， 请勿修改)
%%%
%%%----------------------------------------------
-module(demo_proto_c2s).

%% ! 只关注了 s2c  c2s使用自己手动改一下...
-include("demo_proto.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erl_logger/include/logger.hrl").

-ifndef(CRYPTOR).
-define(CRYPTOR, cryptor).
-endif.
-define(TYPE, c2s).
-define(PROTO_CONVERT(Section, Type), demo_proto_convert:id_mf_convert(Section, Type)).

-export([de_packet/1, en_packet/2]).

%% @doc 对数据包进行解压
de_packet(<<Section:?SECTION_SIZE, Method:?METHOD_SIZE, Rest/binary>>) ->
    % 根据协议中的section和method属性获取对应的模块和函数.
    % 序列号也作为msgid一部分
    MsgID = {Section, Method},
    Type = ?PROTO_CONVERT(MsgID, mod),
    {_Mod, Proto, Fun} = ?PROTO_CONVERT(MsgID, {de, ?TYPE}),
    {ok, {Req, _RemainBytes}, _RemainSize} = Proto:Fun(byte_size(Rest), Rest),
    {ok, {Type, Req}};
de_packet(_Packet) ->
    error(packet_broken).

%% @doc 对消息进行编码,返回iodata
en_packet({Section, Method} = MsgId, Record) ->
    % 消息编码成二进制
    {_Mod, ProtoMod, F} = ?PROTO_CONVERT(MsgId, {en, ?TYPE}),
    Bin = ProtoMod:F(Record),
    Size = iolist_size(Bin),
    ?IF(Size < 16#ffff - 3,
        pass,
        ?ERROR("proto size too large, msgid: ~p, record: ~p", [MsgId, Record])
    ),
    ?assert(Size =< 16#ffff - 3),
    Size2 = Size + ?SECTION_SIZE div 8 + ?METHOD_SIZE div 8,
    {[<<Section:?SECTION_SIZE, Method:?METHOD_SIZE>>, Bin], Size2}.