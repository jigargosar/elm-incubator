module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD



-- Grid


type Grid
    = Grid (List String)


gridInit : Grid
gridInit =
    Grid
        [ "#..."
        , ".#.e"
        , "e..."
        , "..3."
        ]


gridToList : Grid -> List String
gridToList (Grid l) =
    l


gridMoveLeft : Grid
gridMoveLeft =
    Grid
        [ "#..."
        , ".#.e"
        , "e..."
        , ".3.."
        ]


gridMoveRight : Grid
gridMoveRight =
    Grid
        [ "#..."
        , ".#.e"
        , "e..."
        , "...3"
        ]



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
                    ( gridMoveLeft, Cmd.none )

                "ArrowRight" ->
                    ( gridMoveRight, Cmd.none )

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
