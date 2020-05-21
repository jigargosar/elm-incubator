module HtmlStyled exposing (Style, column, el, row)

import Html exposing (Attribute, Html, div)
import Html.Attributes as HA


type Style
    = Batch (List Style)
    | IPX String Int
    | CLASS String


toAttrs : List Style -> List (Attribute Never)
toAttrs =
    let
        toAttrsHelp style =
            case style of
                Batch xs ->
                    List.concatMap toAttrsHelp xs

                IPX n i ->
                    [ HA.style n (String.fromInt i ++ "px") ]

                CLASS cn ->
                    [ HA.class cn ]
    in
    List.concatMap toAttrsHelp


el : List Style -> List (Attribute msg) -> Html msg -> Html msg
el styles attrs child =
    styled div styles attrs [ child ]


row : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
row styles =
    styled div (CLASS "flex flex-row" :: styles)


column : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
column styles =
    styled div (CLASS "flex flex-column" :: styles)


styled : (List (Attribute msg) -> a) -> List Style -> List (Attribute msg) -> a
styled constructor styles attrs =
    constructor (List.map (HA.map never) (toAttrs styles) ++ attrs)
