-module(player).

-include("player.hrl").

-type id() :: term().
-type player() :: #player{}.

-export_type([id/0, player/0]).
