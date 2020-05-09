module Cons exposing (Cons, fromList, fromTail, init, tail)


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


tail : Cons a -> List a
tail =
    Tuple.second


fromList : List a -> Maybe (Cons a)
fromList list =
    case list of
        [] ->
            Nothing

        a :: b ->
            Just (init a b)


fromTail : Cons a -> Maybe (Cons a)
fromTail =
    tail >> fromList
