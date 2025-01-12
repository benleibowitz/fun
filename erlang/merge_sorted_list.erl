-module(merge_sorted_list).
-export([merge/2]).

merge(L, R) -> merge_tail(L, R, []).

%% Tail recursive
merge_tail([], [], Acc) -> lists:reverse(Acc);
merge_tail([H|T], [], Acc) -> merge_tail(T, [], [H|Acc]);
merge_tail([], [H|T], Acc) -> merge_tail(T, [], [H|Acc]);
merge_tail([H1|T1], [H2|T2], Acc) ->
    if
        H1 < H2 -> merge_tail(T1, [H2|T2], [H1|Acc]);
        true -> merge_tail([H1|T1], T2, [H2|Acc])
    end.

%% Body recursive
merge_body([], []) -> [];
merge_body(L, []) ->
    [H|T] = L,
    [H|merge_body(T, [])];
merge_body([], R) -> merge_body(R, []);
merge_body([H1|T1], [H2|T2]) ->
    if
        H1 < H2 -> [H1|merge_body(T1, [H2|T2])];
        true -> [H2|merge_body([H1|T1], T2)]
    end.
