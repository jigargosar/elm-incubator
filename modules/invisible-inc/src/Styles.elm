module Styles exposing (bwi, hi, shadows, wi)

import Html.Attributes exposing (style)
import More exposing (..)


bwi =
    ipx >> style "border-width"


wi =
    ipx >> style "width"


hi =
    ipx >> style "height"


ipx i =
    String.fromInt i ++ "px"


shadows =
    join ", " >> style "box-shadow"
