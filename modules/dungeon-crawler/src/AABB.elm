module AABB exposing (AABB, clampPoint, fromSize, grow, maxXY, minXY, setXY, shift, shrink)

import Basics.More exposing (..)
import Tuple.More as Tuple


type AABB
    = AABB Float2 Float2


zeroXY : Float2
zeroXY =
    ( 0, 0 )


fromPointSize : Float2 -> Float2 -> AABB
fromPointSize xy size =
    AABB xy (Tuple.abs size)


fromSize : Float2 -> AABB
fromSize size =
    fromPointSize zeroXY size


minXY : AABB -> Float2
minXY (AABB xy wh) =
    wh
        |> Tuple.scale -0.5
        |> Tuple.add xy


maxXY : AABB -> Float2
maxXY (AABB xy wh) =
    wh
        |> Tuple.scale 0.5
        |> Tuple.add xy


mapSize : (Float2 -> Float2) -> AABB -> AABB
mapSize f (AABB xy size) =
    AABB xy (f size)


mapXY : (Float2 -> Float2) -> AABB -> AABB
mapXY f (AABB xy size) =
    AABB (f xy) size


shift : Float2 -> AABB -> AABB
shift dxy =
    mapXY (\xy -> Tuple.add xy dxy)


setXY : Float2 -> AABB -> AABB
setXY xy =
    mapXY (always xy)


grow : Float2 -> AABB -> AABB
grow dwh =
    mapSize (\wh -> Tuple.add wh dwh)


shrink : Float2 -> AABB -> AABB
shrink dwh =
    grow (Tuple.negate dwh)


clampPoint : Float2 -> AABB -> Float2
clampPoint pt aabb =
    pt
        |> Tuple.atLeast (minXY aabb)
        |> Tuple.atMost (maxXY aabb)
