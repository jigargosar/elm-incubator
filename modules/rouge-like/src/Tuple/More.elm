module Tuple.More exposing
    ( add
    , andMap
    , any
    , fromFloat
    , join
    , map
    , map2
    , sub
    , toFloat
    , toFloatScaled
    , zero
    )

import Basics.More as Basics exposing (..)


zero : ( number, number )
zero =
    ( 0, 0 )


any : (a -> Bool) -> ( a, a ) -> Bool
any f ( a, b ) =
    f a || f b


map : (a -> x) -> ( a, a ) -> ( x, x )
map f =
    Tuple.mapBoth f f


toFloatScaled : Float -> Int2 -> Float2
toFloatScaled s =
    map (Basics.toFloatScaled s)


toFloat : ( Int, Int ) -> ( Float, Float )
toFloat =
    map Basics.toFloat


fromFloat : ( Float, Float ) -> ( String, String )
fromFloat =
    map String.fromFloat


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub =
    map2 Basics.sub


add : ( number, number ) -> ( number, number ) -> ( number, number )
add =
    map2 Basics.add


andMap : ( a, a ) -> ( a -> b, a -> b ) -> ( b, b )
andMap =
    map2 (|>)


join : appendable -> ( appendable, appendable ) -> appendable
join sep ( a, b ) =
    a ++ sep ++ b
