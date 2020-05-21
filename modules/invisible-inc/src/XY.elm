module XY exposing (XY, currentTargetOffsetXYDecoder, negate, pageXYDecoder, sub)

import Json.Decode as JD exposing (Decoder)


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


pageXYDecoder : Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


currentTargetOffsetXYDecoder : Decoder XY
currentTargetOffsetXYDecoder =
    JD.field "currentTarget"
        (JD.map2 XY
            (JD.field "offsetLeft" JD.float)
            (JD.field "offsetTop" JD.float)
        )
