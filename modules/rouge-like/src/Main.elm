module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD



-- Grid


type Grid
    = Grid {}


gridInit : Grid
gridInit =
    Grid {}


gridToList : Grid -> List String
gridToList (Grid g) =
    []


gridMoveLeft : Grid -> Grid
gridMoveLeft (Grid g) =
    Grid g


gridMoveRight : Grid -> Grid
gridMoveRight (Grid g) =
    Grid g



-- Model


type alias Model =
    Grid


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( gridInit
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            case key of
                "ArrowLeft" ->
                    ( gridMoveLeft model, Cmd.none )

                "ArrowRight" ->
                    ( gridMoveRight model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        ]



-- View


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "code f1" ]
            (model
                |> gridToList
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
