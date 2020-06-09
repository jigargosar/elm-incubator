module Styles exposing
    ( dominantBaselineCentral
    , dominantBaselineHanging
    , fade
    , fill
    , fillBlack
    , fillBlackA
    , fillWhite
    , fillWhiteA
    , height
    , heightPct
    , noEvents
    , noFill
    , noSelect
    , r
    , rx
    , rx100
    , rxPct
    , scale
    , stroke
    , strokeBlack
    , strokeBlackA
    , strokeWhite
    , strokeWhiteA
    , strokeWidth
    , textAnchorMiddle
    , transforms
    , translate
    , translateX
    , translateY
    , width
    , widthPct
    , x
    , y
    )

import Basics.More exposing (..)
import Tuple.More as Tuple
import VirtualDom exposing (Attribute)


translate : Float2 -> String
translate dxy =
    dxy
        |> Tuple.map pxFromFloat
        |> Tuple.join ","
        |> paren
        |> append "translate"


translateX n =
    translate ( n, 0 )


translateY n =
    translate ( 0, n )


scale : Float -> String
scale s =
    String.fromFloat s
        |> paren
        |> append "scale"


fade : Float -> Attribute msg
fade =
    numStyle "opacity"


transforms : List String -> Attribute msg
transforms xs =
    style "transform" (spaced xs)


width : Float -> Attribute msg
width =
    pxStyle "width"


height : Float -> Attribute msg
height =
    pxStyle "height"


widthPct : Float -> Attribute msg
widthPct =
    pctStyle "width"


heightPct : Float -> Attribute msg
heightPct =
    pctStyle "height"


fill : String -> Attribute msg
fill =
    style "fill"


stroke : String -> Attribute msg
stroke =
    style "stroke"


strokeWidth =
    pxStyle "strokeWidth"


fillBlack : Attribute msg
fillBlack =
    fill "black"


fillBlackA : Float -> Attribute msg
fillBlackA =
    blackA >> fill


fillWhite : Attribute msg
fillWhite =
    fill "white"


fillWhiteA : Float -> Attribute msg
fillWhiteA =
    whiteA >> fill


strokeBlack : Attribute msg
strokeBlack =
    stroke "black"


strokeBlackA : Float -> Attribute msg
strokeBlackA =
    blackA >> stroke


strokeWhite : Attribute msg
strokeWhite =
    stroke "white"


strokeWhiteA : Float -> Attribute msg
strokeWhiteA =
    whiteA >> stroke


textAnchor : String -> Attribute msg
textAnchor =
    style "text-anchor"


textAnchorMiddle =
    textAnchor "middle"


dominantBaselineCentral =
    dominantBaseline "central"


dominantBaselineHanging : Attribute msg
dominantBaselineHanging =
    dominantBaseline "hanging"


dominantBaseline : String -> Attribute msg
dominantBaseline =
    style "dominant-baseline"


blackA : Float -> String
blackA a =
    "rgba(0,0,0," ++ fromFloat a ++ ")"


whiteA : Float -> String
whiteA a =
    "rgba(255,255,255," ++ fromFloat a ++ ")"


noFill : Attribute msg
noFill =
    fill "none"


x : Float -> Attribute msg
x =
    pxStyle "x"


y : Float -> Attribute msg
y =
    pxStyle "y"


r : Float -> Attribute msg
r =
    pxStyle "r"


rx : Float -> Attribute msg
rx =
    pxStyle "rx"


rxPct : Float -> Attribute msg
rxPct =
    pctStyle "rx"


rx100 : Attribute msg
rx100 =
    rxPct 100


noSelect : Attribute msg
noSelect =
    style "user-select" "none"


noEvents : Attribute msg
noEvents =
    style "pointer-events" "none"



-- Helpers


pxStyle : String -> Float -> Attribute msg
pxStyle key value =
    style key (pxFromFloat value)


numStyle : String -> Float -> Attribute msg
numStyle key value =
    style key (fromFloat value)


pctStyle : String -> Float -> Attribute msg
pctStyle key value =
    style key (pxFromFloat value)
