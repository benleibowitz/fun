-module(valid_paren).
-export([valid_paren/1]).

valid_paren(X) -> valid_paren(X, []).

valid_paren([], []) -> true;
valid_paren([], _) -> false;
valid_paren([H|T], Acc) ->
    case H of
        40 -> valid_paren(T, ['('|Acc]);
        41 ->
            case Acc of
                [] -> false;
                [H_a|T_a] ->
                    if
                        H_a == '(' -> valid_paren(T, T_a);
                        true -> false
                    end
            end;
        _ -> false
    end.
