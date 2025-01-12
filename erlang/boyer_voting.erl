-module(boyer_voting).
-export([majority/1]).

majority(X) -> majority(X, 0, nil).
majority([], _, Element) -> Element;
majority([H|T], Count, Element) ->
    if
        Count == 0 -> majority(T, 1, H);
        H == Element -> majority(T, Count + 1, Element);
        true -> majority(T, Count - 1, Element)
    end.
