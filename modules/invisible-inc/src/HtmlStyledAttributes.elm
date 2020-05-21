module HtmlStyledAttributes exposing (Style, absolute, batch, class, intPx, string, toAttrs)

import Html exposing (Attribute)
import Html.Attributes as HA


type Style
    = Batch (List Style)
    | IPX String Int
    | CLASS String
    | STRING String String


intPx : String -> Int -> Style
intPx =
    IPX


class : String -> Style
class =
    CLASS


batch : List Style -> Style
batch =
    Batch


absolute : Style
absolute =
    class "absolute"


string : String -> String -> Style
string =
    STRING


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

                STRING n v ->
                    [ HA.style n v ]
    in
    List.concatMap toAttrsHelp
