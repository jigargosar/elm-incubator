module Cons exposing (Cons, fromList, init, map, toList)

import Basics.More exposing (..)
import List.Extra as List


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


fromList : List a -> Maybe (Cons a)
fromList =
    List.uncons


map : (a -> b) -> Cons a -> Cons b
map f =
    Tuple.mapBoth f (List.map f)


toList : Cons a -> List a
toList =
    uncurry (::)
