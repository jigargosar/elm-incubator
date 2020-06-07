module HtmlStyle exposing (..)

import Basics.More exposing (..)
import Html.Attributes exposing (style)
import Tuple.More as Tuple


cssTranslate : ( Float, Float ) -> String
cssTranslate dxy =
    dxy
        |> Tuple.map pxFromFloat
        |> Tuple.join ","
        |> paren
        |> append "translate"


cssScale s =
    String.fromFloat s
        |> paren
        |> append "scale"


cssOpacity o =
    style "opacity" (String.fromFloat o)


cssTransform xs =
    style "transform" (String.join " " xs)
