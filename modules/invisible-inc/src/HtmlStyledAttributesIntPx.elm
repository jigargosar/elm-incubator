module HtmlStyledAttributesIntPx exposing (height, width)

import HtmlStyledAttributes as HSA exposing (Style)


width : Int -> Style
width =
    HSA.intPx "width"


height : Int -> Style
height =
    HSA.intPx "height"
