module HtmlStyledAttributes exposing (Style, class, intPx, toAttrs)

import Html exposing (Attribute)
import Html.Attributes as HA


type Style
    = Batch (List Style)
    | IPX String Int
    | CLASS String


intPx : String -> Int -> Style
intPx =
    IPX


class : String -> Style
class =
    CLASS


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
