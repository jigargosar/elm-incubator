module MouseEvents exposing (click, down, over)

import Html exposing (Attribute)
import Html.Events as HE
import Json.Decode exposing (Decoder)


click : Decoder msg -> Attribute msg
click =
    HE.on "click"


down : Decoder msg -> Attribute msg
down =
    HE.on "mousedown"


over : Decoder msg -> Attribute msg
over =
    HE.on "mouseover"
