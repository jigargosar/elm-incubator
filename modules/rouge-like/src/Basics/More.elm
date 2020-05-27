module Basics.More exposing (..)


allPass : List (a -> Bool) -> a -> Bool
allPass fs v =
    List.all ((|>) v) fs


filterValue : (a -> Bool) -> a -> Maybe a
filterValue isOk x =
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
