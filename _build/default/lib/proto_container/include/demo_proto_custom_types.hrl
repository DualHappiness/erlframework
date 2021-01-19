%%%----------------------------------------------
%%%
%%% @doc: lq{ (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_CUSTOM_TYPES_HRL).
-define(demo_proto_CUSTOM_TYPES_HRL, true).

-record(p_player, {id :: non_neg_integer() ,name :: iolist() | atom() | binary() ,sex :: non_neg_integer() ,lvl :: non_neg_integer() ,ip :: iolist() | atom() | binary() }).
-record(p_item, {id :: non_neg_integer() ,count :: non_neg_integer() }).

-endif. % demo_proto_CUSTOM_TYPES_HRL