module Cons exposing (Cons, init, last, singleton, toList)

import List.Extra as List


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


singleton : a -> Cons a
singleton x =
    init x []


toList : Cons a -> List a
toList ( x, xs ) =
    x :: xs


last : Cons a -> a
last ( x, xs ) =
    List.last xs |> Maybe.withDefault x
