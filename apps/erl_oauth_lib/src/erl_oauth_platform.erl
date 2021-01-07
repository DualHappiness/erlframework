-module(erl_oauth_platform).

-include("erl_oauth_lib.hrl").

-type auth_error() :: unsupport.
-type auth_ret() :: {ok, #auth_ret{}} | {error, Reason :: auth_error()}.

-export_type([auth_ret/0]).

-callback auth(Req :: #auth_body{}) -> auth_ret().

