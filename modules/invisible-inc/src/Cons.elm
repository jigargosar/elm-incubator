module Cons exposing (Cons, init, toList)


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


toList : Cons a -> List a
toList ( x, xs ) =
    x :: xs
