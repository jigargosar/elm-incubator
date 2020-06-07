module HtmlStyle exposing (move, opacity, scale, transforms)

import Basics.More exposing (..)
import Html.Attributes exposing (style)
import Tuple.More as Tuple


move : ( Float, Float ) -> String
move dxy =
    dxy
        |> Tuple.map pxFromFloat
        |> Tuple.join ","
        |> paren
        |> append "translate"


scale s =
    String.fromFloat s
        |> paren
        |> append "scale"


opacity o =
    style "opacity" (String.fromFloat o)


transforms xs =
    style "transform" (String.join " " xs)
