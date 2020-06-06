module Cons exposing (Cons, fromList, init, map, mapAccuml, toList)

import Basics.More exposing (..)
import List.Extra as List


type alias Cons a =
    ( a, List a )


init : a -> List a -> Cons a
init =
    Tuple.pair


singleton : a -> Cons a
singleton x =
    init x []


push : a -> Cons a -> Cons a
push x cons =
    ( x, toList cons )


fromList : List a -> Maybe (Cons a)
fromList =
    List.uncons


append : Cons a -> Cons a -> Cons a
append c d =
    let
        step x xs =
            init x <| toList xs
    in
    foldr step d c


foldr : (a -> b -> b) -> b -> Cons a -> b
foldr f x =
    toList >> List.foldr f x


foldl : (a -> b -> b) -> b -> Cons a -> b
foldl f x =
    toList >> List.foldl f x


appendToList : List a -> Cons a -> Cons a
appendToList l d =
    case fromList l of
        Nothing ->
            d

        Just c ->
            append c d


reverse : Cons a -> Cons a
reverse ( first, rest ) =
    appendToList (List.reverse rest) <| singleton first


map : (a -> b) -> Cons a -> Cons b
map f =
    Tuple.mapBoth f (List.map f)


toList : Cons a -> List a
toList =
    uncurry (::)


mapAccuml : (a -> b -> ( a, c )) -> a -> Cons b -> ( a, Cons c )
mapAccuml f acc0 ( x0, xs ) =
    let
        iAcc =
            f acc0 x0
                |> Tuple.mapSecond singleton

        reducer x ( acc, yCons ) =
            f acc x
                |> Tuple.mapSecond (\y -> push y yCons)

        ( accFinal, generatedCons ) =
            List.foldl reducer iAcc xs
    in
    ( accFinal, reverse generatedCons )
