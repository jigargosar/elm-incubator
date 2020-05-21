module Cons exposing (Cons, init, last, toList)

import List.Extra as List


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


toList : Cons a -> List a
toList ( x, xs ) =
    x :: xs


last : Cons a -> a
last ( x, xs ) =
    List.last xs |> Maybe.withDefault x
