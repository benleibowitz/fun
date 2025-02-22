-module(algo).
-export([merge_sort/1, split/1, sum_list/1, filter/2]).

merge_sort(InList) when length(InList) =< 1 ->
    InList;
merge_sort(InList) ->
    {L, R} = split(InList, []),
    LSplit = merge_sort(L),
    RSplit = merge_sort(R),
    merge(LSplit, RSplit, []).

% Merge 2 sorted arrays
merge([], [], Acc) ->
    lists:reverse(Acc);
merge(L1, [], Acc) ->
    [H|T] = L1,
    merge(T, [], [H|Acc]);
merge([], L2, Acc) ->
    [H|T] = L2,
    merge([], T, [H|Acc]);
merge(L1, L2, Acc) ->
    [H1|T1] = L1,
    [H2|T2] = L2,

    case H1 < H2 of
        true ->
            merge(T1, L2, [H1|Acc]);
        false ->
            merge(L1, T2, [H2|Acc])
    end.

% Splits array into 2 subarrays.
% If array has odd length, left subarray will have 1 more
% item than right subarray.
split(InArr) ->
    split(InArr, []).

split(InArr, LArr)  when length(InArr) > length(LArr) ->
    [H|T] = InArr,
    split(T, [H|LArr]);
split(InArr, LArr) ->
    {lists:reverse(LArr), InArr}.

sum_list(X) -> sum_list(X, 0).

sum_list([], Acc) -> Acc;
sum_list([H|T], Acc) -> sum_list(T, Acc + H).

filter(X, Fn) -> filter(X, Fn, []).

filter([], _, Acc) -> {ok, lists:reverse(Acc)};
filter([H|T], Fn, Acc) ->
    case Fn(H) of
        true -> filter(T, Fn, [H|Acc]);
        false -> filter(T, Fn, Acc);
        _ -> error
    end.
