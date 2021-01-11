-module(gen_mod).

%% PLAYER 代表的是session的数据
%% 严格区分本地和远程调用
%% 远程调用没有player
%% 对一个player数据的修改 必须经过相关player_data
