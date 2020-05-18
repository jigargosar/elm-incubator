module More exposing (..)

-- Math


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
    List.any ((|>) x) fs


allPass : List (a -> Bool) -> a -> Bool
allPass fs x =
    List.all ((|>) x) fs



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



-- Function


flip : (a -> b -> x) -> b -> a -> x
flip f b a =
    f a b
