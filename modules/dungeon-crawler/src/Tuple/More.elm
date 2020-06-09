module Tuple.More exposing
    ( add
    , andMap
    , any
    , fromFloat
    , fromInt
    , halve
    , join
    , map
    , map2
    , mul
    , negate
    , repeat
    , scale
    , spaced
    , spacedFloats
    , sub
    , toFloat
    , toFloatScaled
    , zero
    )

import Basics.More as B exposing (..)


zero : ( number, number )
zero =
    repeat 0


repeat : a -> ( a, a )
repeat =
    twice


any : (a -> Bool) -> ( a, a ) -> Bool
any f ( a, b ) =
    f a || f b


map : (a -> x) -> ( a, a ) -> ( x, x )
map f =
    Tuple.mapBoth f f


toFloatScaled : Float -> Int2 -> Float2
toFloatScaled s =
    map (B.toFloatScaled s)


toFloat : Int2 -> Float2
toFloat =
    map B.toFloat


fromFloat : Float2 -> String2
fromFloat =
    map B.fromFloat


spaced : String2 -> String
spaced =
    join " "


spacedFloats : Float2 -> String
spacedFloats =
    fromFloat >> spaced


fromInt : Int2 -> ( String, String )
fromInt =
    map B.fromInt


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub =
    map2 B.sub


add : ( number, number ) -> ( number, number ) -> ( number, number )
add =
    map2 B.add


scale : number -> ( number, number ) -> ( number, number )
scale s =
    map (B.mul s)


halve : Float2 -> Float2
halve =
    scale 0.5


negate : ( number, number ) -> ( number, number )
negate =
    map B.negate


mul : ( number, number ) -> ( number, number ) -> ( number, number )
mul =
    map2 B.mul


andMap : ( a, a ) -> ( a -> b, a -> b ) -> ( b, b )
andMap =
    map2 (|>)


join : appendable -> ( appendable, appendable ) -> appendable
join sep ( a, b ) =
    a ++ sep ++ b
