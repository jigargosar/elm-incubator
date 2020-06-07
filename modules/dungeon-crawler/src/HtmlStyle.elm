module HtmlStyle exposing (move, opacity, scale, transforms)

import Basics.More exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Tuple.More as Tuple


move : ( Float, Float ) -> String
move dxy =
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
