module Draw exposing
    ( canvas
    , square, rect, circle
    , Op, fade, move, scale
    , class, group, noTransition, rotate, transition
    )

{-|

@docs canvas
@docs square, rect, circle
@docs Op, fade, move, scale

-}

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg, rect)
import Svg.Attributes as SA
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Attributes.InPx exposing (height, r, width)
import TypedSvg.Types as T



-- PUBLIC API


canvas : Float -> Float -> List (Svg msg) -> Html msg
canvas w h =
    let
        x =
            w * -0.5

        y =
            h * -0.5
    in
    Svg.svg [ viewBox x y w h, width w, height h, Html.Attributes.style "display" "flex" ]


square : String -> Float -> List Op -> Svg msg
square c w =
    rect c w w


rect : String -> Float -> Float -> List Op -> Svg msg
rect color w h ops =
    initRect color w h |> applyOps ops |> renderRect


circle : String -> Float -> List Op -> Svg msg
circle color r ops =
    initCircle color r |> applyOps ops |> renderCircle


group : List Op -> List (Svg msg) -> Svg msg
group ops list =
    initGroup |> applyOps ops |> renderGroup list


type Op
    = Fade Float
    | Move Float Float
    | Scale Float
    | Rotate Float
    | Transition String
    | Class String


type alias OpRec a =
    { a | a : Float, classes : List String, o : Float, s : Float, trans : String, x : Float, y : Float }


applyOps :
    List Op
    -> OpRec a
    -> OpRec a
applyOps ops record =
    List.foldl applyOp record ops


applyOp :
    Op
    -> OpRec a
    -> OpRec a
applyOp op ({ x, y, s, a } as m) =
    case op of
        Fade o ->
            { m | o = o }

        Move dx dy ->
            { m | x = x + dx, y = y + dy }

        Scale ns ->
            { m | s = s * ns }

        Rotate da ->
            { m | a = a + da }

        Transition trans ->
            { m | trans = trans }

        Class cn ->
            { m | classes = cn :: m.classes }


fade =
    Fade


move =
    Move


scale =
    Scale


rotate =
    Rotate


noTransition =
    Transition "none"


transition =
    Transition


class =
    Class



-- SVG PRIVATE API


type alias Rect =
    { x : Float
    , y : Float
    , s : Float
    , a : Float
    , o : Float
    , trans : String
    , classes : List String
    , fill : String
    , w : Float
    , h : Float
    }


type alias Circle =
    { x : Float
    , y : Float
    , s : Float
    , a : Float
    , o : Float
    , trans : String
    , classes : List String
    , fill : String
    , r : Float
    }


type alias Group =
    { x : Float
    , y : Float
    , s : Float
    , a : Float
    , o : Float
    , trans : String
    , classes : List String
    }


initRect : String -> Float -> Float -> Rect
initRect =
    Rect 0 0 1 0 1 defaultTransition []


initCircle : String -> Float -> Circle
initCircle =
    Circle 0 0 1 0 1 defaultTransition []


initGroup : Group
initGroup =
    Group 0 0 1 0 1 defaultTransition []


renderRect : Rect -> Svg msg
renderRect m =
    Svg.rect
        [ width m.w
        , height m.h
        , SA.class (renderClasses m.classes)
        , renderStyles
            [ fill m.fill
            , transform <| renderRectTransform m
            , opacity m.o
            , renderTransition m.trans
            ]
        ]
        []


renderClasses =
    List.reverse >> String.join " "


renderTransition =
    Tuple.pair "transition"


renderCircle : Circle -> Svg msg
renderCircle m =
    Svg.circle
        [ r m.r
        , SA.class (renderClasses m.classes)
        , renderStyles
            [ fill m.fill
            , transform <| renderTransform m
            , opacity m.o
            , renderTransition m.trans
            ]
        ]
        []


renderGroup : List (Svg msg) -> Group -> Svg msg
renderGroup children m =
    Svg.g
        [ SA.class (renderClasses m.classes)
        , renderStyles
            [ transform <| renderTransform m
            , opacity m.o
            , renderTransition m.trans
            ]
        ]
        children


defaultTransition =
    "all 500ms"


renderStyles : List ( String, String ) -> Svg.Attribute msg
renderStyles list =
    SA.style
        ([--( "transition", defaultTransition )
          --, ( "transition", "none" )
         ]
            ++ list
            |> List.map styleTupleToString
            |> String.join ""
        )


renderRectTransform m =
    T.Translate (m.w * -0.5) (m.h * -0.5) :: renderTransform m


renderTransform m =
    [ T.Translate m.x m.y, T.Scale m.s m.s, T.Rotate m.a 0 0 ]


fill : String -> ( String, String )
fill =
    Tuple.pair "fill"


opacity : Float -> ( String, String )
opacity o =
    ( "opacity", String.fromFloat o )


transform : List T.Transform -> ( String, String )
transform transforms =
    ( "transform", String.join " " (List.map transformToString transforms) )


transformToString : T.Transform -> String
transformToString xform =
    let
        trNum name args =
            tr name (List.map num args)

        trPx name args =
            tr name (List.map px args)

        tr name args =
            String.concat
                [ name
                , "("
                , String.join "," args
                , ")"
                ]

        px f =
            String.fromFloat f ++ "px"

        num f =
            String.fromFloat f

        deg f =
            String.fromFloat f ++ "deg"
    in
    case xform of
        T.Matrix a b c d e f ->
            trNum "matrix" [ a, b, c, d, e, f ]

        T.Rotate a _ _ ->
            tr "rotate" [ deg a ]

        T.Scale x y ->
            trNum "scale" [ x, y ]

        T.SkewX x ->
            trNum "skewX" [ x ]

        T.SkewY y ->
            trNum "skewY" [ y ]

        T.Translate x y ->
            trPx "translate" [ x, y ]


styleTupleToString : ( String, String ) -> String
styleTupleToString ( n, v ) =
    n ++ ":" ++ v ++ ";"
