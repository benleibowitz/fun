-module(two_sum).
-export([two_sum/2]).

two_sum(Arr, Target) -> two_sum(Arr, Target, #{}, 0).

two_sum([], _, _, _) -> error;
two_sum([H|T], Target, Seen, Idx1) ->
    case maps:find(Target - H, Seen) of
        {ok, Idx2} -> {ok, [Idx2, Idx1]};
        error -> two_sum(T, Target, maps:put(H, Idx1, Seen), Idx1 + 1)
    end.
