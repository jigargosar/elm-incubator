module Styles exposing (fill, height, opacity, scale, transforms, translate, width, widthPct)

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


widthPct : Float -> Attribute msg
widthPct =
    pctStyle "width"


height : Float -> Attribute msg
height =
    pxStyle "height"


pxStyle : String -> Float -> Attribute msg
pxStyle key value =
    style key (pxFromFloat value)


numStyle : String -> Float -> Attribute msg
numStyle key value =
    style key (fromFloat value)


pctStyle : String -> Float -> Attribute msg
pctStyle key value =
    style key (pxFromFloat value)


fill : String -> Attribute msg
fill =
    style "fill"
