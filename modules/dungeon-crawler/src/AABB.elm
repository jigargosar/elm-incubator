module AABB exposing (AABB, fromSize, grow, shrink)

import Basics.More exposing (..)
import Tuple.More as Tuple


type AABB
    = AABB Float2 Float2


zeroXY : Float2
zeroXY =
    ( 0, 0 )


fromSize : Float2 -> AABB
fromSize size =
    AABB zeroXY size


mapSize : (Float2 -> Float2) -> AABB -> AABB
mapSize f (AABB xy size) =
    AABB xy (f size)


grow : Float2 -> AABB -> AABB
grow dwh =
    mapSize (\wh -> Tuple.add wh dwh)


shrink : Float2 -> AABB -> AABB
shrink dwh =
    grow (Tuple.negate dwh)
