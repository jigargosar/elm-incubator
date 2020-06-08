module Styles exposing (height, opacity, scale, transforms, translate, width)

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
opacity o =
    style "opacity" (fromFloat o)


transforms : List String -> Attribute msg
transforms xs =
    style "transform" (spaced xs)


width : Float -> Attribute msg
width n =
    style "width" (pxFromFloat n)


height : Float -> Attribute msg
height n =
    style "height" (pxFromFloat n)
