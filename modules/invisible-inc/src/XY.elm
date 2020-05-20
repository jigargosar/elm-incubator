module XY exposing (XY, negate)


type alias XY =
    { x : Float, y : Float }


negate : XY -> XY
negate =
    mapBoth Basics.negate


mapEach : (Float -> Float) -> (Float -> Float) -> XY -> XY
mapEach fx fy xy =
    XY (fx xy.x) (fy xy.y)


mapBoth : (Float -> Float) -> XY -> XY
mapBoth f =
    mapEach f f
