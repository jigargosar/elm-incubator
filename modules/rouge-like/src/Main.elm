module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- Model


type alias Model =
    List String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( [ "#..."
      , ".#.e"
      , "e..."
      , "...3"
      ]
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


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "code f1" ]
            (model
                |> List.map (\s -> div [] [ text s ])
            )
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
