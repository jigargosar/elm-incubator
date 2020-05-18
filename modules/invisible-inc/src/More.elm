module More exposing (..)

-- Math

import Maybe.Extra
import Set exposing (Set)


add : number -> number -> number
add =
    (+)



-- Logic


neq : a -> a -> Bool
neq =
    (/=)


eq : a -> a -> Bool
eq =
    (==)


anyPass : List (a -> Bool) -> a -> Bool
anyPass fs x =
    List.any (applyTo x) fs


allPass : List (a -> Bool) -> a -> Bool
allPass fs x =
    List.all (applyTo x) fs



-- Tuple


pair : a -> b -> ( a, b )
pair =
    Tuple.pair


mapFirst : (a -> x) -> ( a, b ) -> ( x, b )
mapFirst =
    Tuple.mapFirst


mapSecond : (b -> y) -> ( a, b ) -> ( a, y )
mapSecond =
    Tuple.mapSecond


mapEach : (a -> x) -> (b -> y) -> ( a, b ) -> ( x, y )
mapEach =
    Tuple.mapBoth


mapBoth : (a -> x) -> ( a, a ) -> ( x, x )
mapBoth f =
    mapEach f f


fst =
    Tuple.first


snd =
    Tuple.second



-- Maybe


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap =
    Maybe.Extra.unwrap



-- List


prepend : a -> List a -> List a
prepend =
    (::)



-- Function


flip : (a -> b -> x) -> b -> a -> x
flip f b a =
    f a b


applyTo : a -> (a -> b) -> b
applyTo =
    (|>)



-- Set


notMemberOfSet : Set comparable -> comparable -> Bool
notMemberOfSet set x =
    Set.member x set |> not


setRemoveAll : Set comparable -> Set comparable -> Set comparable
setRemoveAll rs s =
    Set.diff s rs
