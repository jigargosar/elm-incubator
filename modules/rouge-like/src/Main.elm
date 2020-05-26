module Main exposing (main)

import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import Position exposing (Position)



-- Model


type alias Model =
    { dimension : Dimension
    , player : Position
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { dimension = Dimension.new 10 18
      , player = Position.new 5 5
      }
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
            case directionFromKey key of
                Just direction ->
                    ( movePlayerInDirection direction model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


movePlayerInDirection : Direction -> Model -> Model
movePlayerInDirection direction model =
    let
        position : Position
        position =
            model.player
                |> stepPositionInDirection direction
    in
    if Dimension.member position model.dimension then
        { model | player = position }

    else
        model


stepPositionInDirection : Direction -> Position -> Position
stepPositionInDirection direction =
    case direction of
        Left ->
            Position.left

        Right ->
            Position.right

        Up ->
            Position.up

        Down ->
            Position.down


directionFromKey : String -> Maybe Direction
directionFromKey key =
    case key of
        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing


type Direction
    = Left
    | Right
    | Up
    | Down


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
            (viewGridRows model.player model.dimension)
        ]


type alias HM =
    Html Msg


viewGridRows : Position -> Dimension -> List HM
viewGridRows playerPosition dimension =
    Dimension.toRows dimension
        |> List.map (viewRow playerPosition)


viewRow playerPosition positions =
    div [] (List.map (viewCell playerPosition) positions)


viewCell playerPosition position =
    if position == playerPosition then
        text "3"

    else
        text "."



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
