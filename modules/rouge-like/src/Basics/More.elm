module Basics.More exposing (..)

import Html
import Html.Attributes
import Random


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


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


dec =
    add -1


add =
    (+)


{-| Defines an upper bound for a variable.

    42 |> atMost 0 --> 0

    -42 |> atMost 0 --> -42

-}
atMost : comparable -> comparable -> comparable
atMost =
    min


{-| Defines a lower bound for a variable.

    -42 |> atLeast 0 --> 0

    42 |> atLeast 0 --> 42

-}
atLeast : comparable -> comparable -> comparable
atLeast =
    max


{-| The maximum _safe_ value for an integer, defined as `2^53 - 1`. Anything
larger than that and behaviour becomes mathematically unsound.

    maxSafeInteger + 1 --> maxSafeInteger + 2

-}
maxNum : number
maxNum =
    2 ^ 53 - 1


{-| The minimum _safe_ value for an integer, defined as `-(2^53 - 1)`. Anything
smaller than that, and behaviour becomes mathematically unsound.

    minSafeInteger - 1 --> minSafeInteger - 2

-}
minNum : number
minNum =
    -maxNum


{-| Swaps the elements in a pair.

    swap ( 1, 2 ) --> ( 2, 1 )

-}
swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


{-| Flip the order of the first two arguments to a function.
-}
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


{-| Change how arguments are passed to a function.
This splits paired arguments into two separate arguments.
-}
curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


with f1 f2 x =
    f2 (f1 x) x


withMaybeAndThen f1 f2 x =
    f1 x
        |> Maybe.andThen (\a -> f2 a x)


applyTo x f =
    f x


sub =
    (-)


mul =
    (*)


toFloatScaled : Float -> Int -> Float
toFloatScaled s i =
    toFloat i * s


absDiff a b =
    a - b |> abs


maybeUniformGenerator : List a -> Maybe (Random.Generator a)
maybeUniformGenerator l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            Random.uniform x xs |> Just


appendWith : appendable -> appendable -> appendable
appendWith b a =
    a ++ b


pxFromFloat : Float -> String
pxFromFloat =
    String.fromFloat >> appendWith "px"


surround : appendable -> appendable -> appendable -> appendable
surround a b x =
    a ++ x ++ b


paren =
    surround "(" ")"


append =
    (++)


attrIf bool x =
    if bool then
        x

    else
        noAttr


attrMaybe mx f =
    case mx of
        Just x ->
            f x

        Nothing ->
            noAttr


noAttr =
    Html.Attributes.classList []


viewIf bool x =
    if bool then
        x

    else
        noView


noView =
    Html.text ""


eq =
    (==)


eqBy f a b =
    f a == f b


eqById =
    eqBy .id


propEq x fx a =
    x == fx a


idEq id =
    propEq id .id


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)


rangeLen2 : Int -> Int -> List (List ( Int, Int ))
rangeLen2 l1 l2 =
    rangeLen l1
        |> List.map (\i1 -> rangeLen l2 |> List.map (Tuple.pair i1))


memberOfLen : Int -> Int -> Bool
memberOfLen len x =
    clamp 0 (len - 1) x == x
