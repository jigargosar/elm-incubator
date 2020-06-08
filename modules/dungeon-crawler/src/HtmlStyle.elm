module HtmlStyle exposing (height, opacity, scale, transforms, translate, width)

import Basics.More exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Tuple.More as Tuple


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
    style "opacity" (String.fromFloat o)


transforms : List String -> Attribute msg
transforms xs =
    style "transform" (String.join " " xs)


width : Float -> Attribute msg
width n =
    style "width" (pxFromFloat n)


height : Float -> Attribute msg
height n =
    style "height" (pxFromFloat n)
