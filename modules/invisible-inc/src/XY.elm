module XY exposing (XY, negate, sub)


type alias XY =
    { x : Float, y : Float }


negate : XY -> XY
negate =
    mapBoth Basics.negate


sub : XY -> XY -> XY
sub a b =
    add a (negate b)


add : XY -> XY -> XY
add a b =
    XY (a.x + b.x) (a.y + b.y)


mapEach : (Float -> Float) -> (Float -> Float) -> XY -> XY
mapEach fx fy xy =
    XY (fx xy.x) (fy xy.y)


mapBoth : (Float -> Float) -> XY -> XY
mapBoth f =
    mapEach f f
