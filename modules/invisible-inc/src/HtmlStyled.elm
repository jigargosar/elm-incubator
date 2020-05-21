module HtmlStyled exposing (Style, column, el, row)

import Html exposing (Attribute, Html, div)
import Html.Attributes as HA
import HtmlStyledAttributes as HSA


type alias Style =
    HSA.Style


el : List Style -> List (Attribute msg) -> Html msg -> Html msg
el styles attrs child =
    styled div styles attrs [ child ]


row : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
row styles =
    styled div (HSA.class "flex flex-row" :: styles)


column : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
column styles =
    styled div (HSA.class "flex flex-column" :: styles)


styled : (List (Attribute msg) -> a) -> List Style -> List (Attribute msg) -> a
styled constructor styles attrs =
    constructor (List.map (HA.map never) (HSA.toAttrs styles) ++ attrs)
