-module(sum_list).
-export([sum_list/2]).

sum_list(L, R) -> sum_list(L, R, []).

sum_list([], [], Acc) -> lists:reverse(Acc);
sum_list([H|T], [], Acc) -> sum_list(T, [], [H|Acc]);
sum_list([], [H|T], Acc) -> sum_list(T, [], [H|Acc]);
sum_list([H1|T1], [H2|T2], Acc) -> sum_list(T1, T2, [H1 + H2|Acc]).
