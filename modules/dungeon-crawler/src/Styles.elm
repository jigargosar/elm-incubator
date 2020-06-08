module Styles exposing (fill, fillBlack, fillBlackA, fillWhite, fillWhiteA, height, heightPct, noFill, opacity, r, rx, rx100, rxPct, scale, transforms, translate, width, widthPct, x, y)

import Basics.More exposing (..)
import Tuple.More as Tuple
import VirtualDom exposing (Attribute)


translate : ( Float, Float ) -> String
translate dxy =
    dxy
        |> Tuple.map pxFromFloat
        |> Tuple.join ","
        |> paren
        |> append "translate"


scale : Float -> String
scale s =
    String.fromFloat s
        |> paren
        |> append "scale"


opacity : Float -> Attribute msg
opacity =
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
