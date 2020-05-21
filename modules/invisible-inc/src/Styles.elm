module Styles exposing (hi, wi)

import Html.Attributes exposing (style)
import More exposing (..)


wi =
    ipx >> style "width"


hi =
    ipx >> style "height"


ipx i =
    String.fromInt i ++ "px"
