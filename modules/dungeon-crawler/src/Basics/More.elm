module Basics.More exposing (..)

import List.Extra as List
import Random exposing (Generator)
import Random.List
import VirtualDom


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


type alias String2 =
    ( String, String )


toFloat =
    Basics.toFloat


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


dec : number -> number
dec =
    add -1


add : number -> number -> number
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


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


with : (c -> a) -> (a -> c -> b) -> c -> b
with f1 f2 x =
    f2 (f1 x) x


withMaybeAndThen : (c -> Maybe a) -> (a -> c -> Maybe b) -> c -> Maybe b
withMaybeAndThen f1 f2 x =
    f1 x
        |> Maybe.andThen (\a -> f2 a x)


applyTo : a -> (a -> b) -> b
applyTo x f =
    f x


sub : number -> number -> number
sub =
    (-)


mul : number -> number -> number
mul =
    (*)


toFloatScaled : Float -> Int -> Float
toFloatScaled s i =
    toFloat i * s


absDiff : number -> number -> number
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
    fromFloat >> withSuffix "px"


pctFromFloat : Float -> String
pctFromFloat =
    fromFloat >> withSuffix "%"


surround : appendable -> appendable -> appendable -> appendable
surround a b x =
    a ++ x ++ b


paren : String -> String
paren =
    surround "(" ")"


join : String -> List String -> String
join =
    String.join


join3 p sep s xs =
    join sep xs
        |> surround p s


append : appendable -> appendable -> appendable
append =
    (++)


prefixed : appendable -> appendable -> appendable
prefixed =
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


class =
    VirtualDom.attribute "class"


ifElse : (d -> Bool) -> (d -> a) -> (d -> a) -> d -> a
ifElse p t f x =
    if p x then
        t x

    else
        f x


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


pairFloat : Float -> Float -> Float2
pairFloat =
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


subscribeIf : Bool -> Sub msg -> Sub msg
subscribeIf bool x =
    if bool then
        x

    else
        noSub


noSub : Sub msg
noSub =
    Sub.none


cmdIf : Bool -> Cmd msg -> Cmd msg
cmdIf bool x =
    if bool then
        x

    else
        noCmd


noCmd : Cmd msg
noCmd =
    Cmd.none


spaced : List String -> String
spaced =
    String.join " "


spacedFromFloat : List Float -> String
spacedFromFloat =
    List.map fromFloat >> spaced


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
