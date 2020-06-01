module Basics.More exposing (..)

import Random


type alias Int2 =
    ( Int, Int )


allPass : List (a -> Bool) -> a -> Bool
allPass fs v =
    List.all ((|>) v) fs


justWhen : (a -> Bool) -> a -> Maybe a
justWhen isOk x =
    if isOk x then
        Just x

    else
        Nothing


ignoreNothing : (b -> Maybe b) -> b -> b
ignoreNothing f x =
    f x |> Maybe.withDefault x


ignoreNothing2 : (b -> a -> Maybe a) -> b -> a -> a
ignoreNothing2 f x =
    ignoreNothing (f x)


pairTo : a -> b -> ( b, a )
pairTo b a =
    Tuple.pair a b


pair =
    Tuple.pair


headOr : a -> List a -> a
headOr default =
    List.head >> Maybe.withDefault default


toGeneratorOrConstant : (b -> Maybe (Random.Generator b)) -> b -> Random.Generator b
toGeneratorOrConstant f x =
    f x |> Maybe.withDefault (Random.constant x)
