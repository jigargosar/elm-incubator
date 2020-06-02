module Tuple.More exposing (andMap, any, apply, map, map2, sub)

import Basics.More as Basics


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


any : (a -> Bool) -> ( a, a ) -> Bool
any f ( a, b ) =
    f a || f b


map : (a -> x) -> ( a, a ) -> ( x, x )
map f =
    Tuple.mapBoth f f


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub =
    map2 Basics.sub


andMap : ( a, a ) -> ( a -> b, a -> b ) -> ( b, b )
andMap =
    map2 (|>)
