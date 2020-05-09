module Cons exposing (Cons, init)


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair
