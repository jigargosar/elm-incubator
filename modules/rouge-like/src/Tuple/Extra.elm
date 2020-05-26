module Tuple.Extra exposing (andMap, apply, map, map2)


map =
    tupleMap


map2 =
    tupleMap2


andMap =
    tupleAndMap


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


tupleMap : (a -> x) -> ( a, a ) -> ( x, x )
tupleMap f =
    Tuple.mapBoth f f


tupleMap2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleMap2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


tupleAndMap : ( a, a ) -> ( a -> b, a -> b ) -> ( b, b )
tupleAndMap =
    tupleMap2 (|>)
