%%%----------------------------------------------
%%%
%%% @doc: test (自动生成， 请勿修改)
%%%
%%%----------------------------------------------

-ifndef(demo_proto_test_HRL).
-define(demo_proto_test_HRL, true).

-record(test_part1_c2s, {value :: non_neg_integer() }).

-record(test_part1_s2c, {value :: non_neg_integer() }).
-record(test_part2_s2c, {msg :: iolist() | atom() | binary() }).

-endif. % demo_proto_test_HRL