module MouseEvents exposing (click, over)

import Html exposing (Attribute)
import Html.Events as HE
import Json.Decode exposing (Decoder)


click : Decoder msg -> Attribute msg
click =
    HE.on "click"


over : Decoder msg -> Attribute msg
over =
    HE.on "mouseover"
