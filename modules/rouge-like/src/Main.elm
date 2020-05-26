module Main exposing (main)

import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import List.Extra as List
import Position exposing (Position)
import Random
import Random.List as Random



-- Model


type alias Model =
    { dimension : Dimension
    , player : Position
    , walls : List Position
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        dimension =
            Dimension.new 10 18

        playerPosition =
            Position.new 5 5

        emptyPositions0 =
            dimension
                |> Dimension.toPositions
                |> List.remove playerPosition

        seed0 =
            Random.initialSeed 0

        ( ( walls, _ ), _ ) =
            Random.step (shuffleSplit 20 emptyPositions0) seed0
    in
    ( { dimension = dimension
      , player = playerPosition
      , walls = walls
      }
    , Cmd.none
    )


shuffleSplit : Int -> List a -> Random.Generator ( List a, List a )
shuffleSplit n xs =
    Random.shuffle xs
        |> Random.map (List.splitAt n)



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
    if
        Dimension.member position model.dimension
            && not (List.member position model.walls)
    then
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
        [ viewGrid model
        ]


type alias HM =
    Html Msg


viewGrid : Model -> HM
viewGrid model =
    let
        viewRow rowString =
            div [] [ text rowString ]
    in
    div [ class "code f1" ]
        (Dimension.toRows model.dimension
            |> List.map (positionsToString model >> viewRow)
        )


positionsToString : Model -> List Position -> String
positionsToString model =
    List.map (positionToChar model) >> String.fromList


positionToChar : Model -> Position -> Char
positionToChar model position =
    if position == model.player then
        '3'

    else if List.member position model.walls then
        '#'

    else
        '.'



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
