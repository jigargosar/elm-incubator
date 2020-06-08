module Basics.More exposing (..)

import List.Extra as List
import Random exposing (Generator)
import Random.List
import VirtualDom


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


pair : a -> b -> ( a, b )
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


atMost : comparable -> comparable -> comparable
atMost =
    min


atLeast : comparable -> comparable -> comparable
atLeast =
    max


maxNum : number
maxNum =
    2 ^ 53 - 1


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


withSuffix : appendable -> appendable -> appendable
withSuffix b a =
    a ++ b


pxFromFloat : Float -> String
pxFromFloat =
    String.fromFloat >> withSuffix "px"


surround : appendable -> appendable -> appendable -> appendable
surround a b x =
    a ++ x ++ b


paren =
    surround "(" ")"


join =
    String.join


join3 p sep s xs =
    join sep xs
        |> surround p s


append : appendable -> appendable -> appendable
append =
    (++)


withPrefix : appendable -> appendable -> appendable
withPrefix =
    append


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
    VirtualDom.style "" ""


style =
    VirtualDom.style


viewIf bool x =
    if bool then
        x

    else
        noView


noView =
    VirtualDom.text ""


eq : a -> a -> Bool
eq =
    (==)


eqBy f a b =
    f a == f b


eqById : { a | id : b } -> { a | id : b } -> Bool
eqById =
    eqBy .id


propEq : a -> (c -> a) -> c -> Bool
propEq x fx a =
    x == fx a


idEq : b -> { a | id : b } -> Bool
idEq id =
    propEq id .id


rangeLen2 : Int -> Int -> List (List Int2)
rangeLen2 l1 l2 =
    times l1 (\i1 -> times l2 (pairInt i1))


isValidIndex : Int -> Int -> Bool
isValidIndex x len =
    x == clamp 0 (len - 1) x


pairInt : Int -> Int -> Int2
pairInt =
    pair


shuffleSplit : Int -> List a -> Generator ( List a, List a )
shuffleSplit n xs =
    Random.List.shuffle xs
        |> Random.andThen Random.List.shuffle
        |> Random.map (List.splitAt n)


removeAll : List a -> List a -> List a
removeAll toRemove =
    let
        shouldKeep x =
            List.notMember x toRemove
    in
    List.filter shouldKeep


subscribeIf bool s =
    if bool then
        s

    else
        noSub


noSub =
    Sub.none


noCmd =
    Cmd.none


spaced : List String -> String
spaced =
    String.join " "


filter : (a -> Bool) -> List a -> List a
filter =
    List.filter


reject : (a -> Bool) -> List a -> List a
reject =
    complement >> filter


complement : (a -> Bool) -> a -> Bool
complement f =
    f >> not


fromFloat =
    String.fromFloat


fromInt =
    String.fromInt


indices n =
    List.range 0 (n - 1)


times n f =
    indices n |> List.map f
