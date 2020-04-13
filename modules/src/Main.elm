module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, div, node, text)
import Html.Attributes exposing (class, href, style)



-- Model


type alias Model =
    { modules : List String }


type alias Flags =
    { modules : List String }


init : Flags -> ( Model, Cmd Msg )
init f =
    ( Model f.modules
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias DM =
    Document Msg


view : Model -> DM
view model =
    Document "Elm Incubator"
        [ styleSheet
        , div [ class "f4 pv2" ] [ text "Elm Incubator Modules" ]
        , div [] (List.map viewLink model.modules)
        ]


viewLink string =
    div [ class "pv2" ]
        [ a
            [ href string
            , class "f5 ttc"
            , style "color" "rgba(43, 43, 43)"
            ]
            [ text (String.replace "-" " " string) ]
        ]


styleSheet =
    node "style"
        []
        [ text styleSheetContent
        ]


styleSheetContent : String
styleSheetContent =
    """
        body {
            max-width: 30em;
            margin: 0 auto;
        }
    """



-- Main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
