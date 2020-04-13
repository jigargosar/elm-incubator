module Draw exposing (Op, circle, fade, move, rect, scale, square)

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
    initRect color w h |> applyOps ops |> renderRect


circle : String -> Float -> List Op -> Svg.Svg msg
circle color r ops =
    initCircle color r |> applyOps ops |> renderCircle


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
applyOp op ({ x, y, s } as m) =
    case op of
        Fade o ->
            { m | o = o }

        Move dx dy ->
            { m | x = x + dx, y = y + dy }

        Scale_ ns ->
            { m | s = s * ns }


fade =
    Fade


move =
    Move


scale =
    Scale



-- SVG PRIVATE API


type alias Rect =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , w : Float
    , h : Float
    }


type alias Circle =
    { x : Float
    , y : Float
    , s : Float
    , o : Float
    , fill : String
    , r : Float
    }


initRect : String -> Float -> Float -> Rect
initRect =
    Rect 0 0 1 1


initCircle : String -> Float -> Circle
initCircle =
    Circle 0 0 1 1


renderRect : Rect -> Svg.Svg msg
renderRect m =
    Svg.rect
        [ width m.w
        , height m.h
        , SA.fill m.fill
        , transform <| renderRectTransform m
        , opacity m.o
        ]
        []


renderCircle : Circle -> Svg.Svg msg
renderCircle m =
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
