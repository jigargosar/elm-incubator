module Draw exposing (circle, fade, move, rect, scale, square)

import Svg exposing (rect)
import Svg.Attributes as SA
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx exposing (height, r, width)
import TypedSvg.Types exposing (Opacity(..), Transform(..))



-- PUBLIC API


square : String -> Float -> List Op -> Svg.Svg msg
square c w =
    rect c w w


rect : String -> Float -> Float -> List Op -> Svg.Svg msg
rect color w h ops =
    initRectRecord color w h |> applyOps ops |> renderRectRecord


circle : String -> Float -> List Op -> Svg.Svg msg
circle color r ops =
    initCircleRecord color r |> applyOps ops |> renderCircleRecord


type Op
    = Fade Float
    | Move Float Float
    | Scale_ Float


applyOps :
    List Op
    -> { a | o : Float, s : Float, x : Float, y : Float }
    -> { a | o : Float, s : Float, x : Float, y : Float }
applyOps ops record =
    List.foldl applyOp record ops


applyOp :
    Op
    -> { a | o : Float, s : Float, x : Float, y : Float }
    -> { a | o : Float, s : Float, x : Float, y : Float }
applyOp op =
    case op of
        Fade o ->
            fadeRecord o

        Move x y ->
            moveRecord x y

        Scale_ s ->
            scaleRecord s


fade =
    Fade


move =
    Move


scale =
    Scale



-- SVG PRIVATE API


fadeRecord o m =
    { m | o = o }


moveRecord dx dy ({ x, y } as m) =
    { m | x = x + dx, y = y + dy }


scaleRecord ns ({ s } as m) =
    { m | s = s * ns }


type alias RectangleRecord =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , w : Float
    , h : Float
    }


type alias CircleRecord =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , r : Float
    }


initRectRecord : String -> Float -> Float -> RectangleRecord
initRectRecord =
    RectangleRecord 0 0 1 1


initCircleRecord : String -> Float -> CircleRecord
initCircleRecord =
    CircleRecord 0 0 1 1



--renderShape : Shape -> Svg.Svg msg
--renderShape shape =
--    case shape of
--        Rectangle m ->
--            renderRectRecord m
--
--        Circle m ->
--            renderCircleRecord m


renderRectRecord : RectangleRecord -> Svg.Svg msg
renderRectRecord m =
    Svg.rect
        [ width m.w
        , height m.h
        , SA.fill m.fill
        , transform <| renderRectTransform m
        , opacity m.o
        ]
        []


renderCircleRecord : CircleRecord -> Svg.Svg msg
renderCircleRecord m =
    Svg.circle
        [ r m.r
        , SA.fill m.fill
        , transform <| renderTransform m
        , opacity m.o
        ]
        []


renderRectTransform m =
    Translate (m.w * -0.5) (m.h * -0.5)
        :: renderTransform m


renderTransform m =
    [ Translate m.x m.y, Scale m.s m.s ]


opacity =
    TypedSvg.Attributes.opacity << Opacity
