module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import Tuple exposing (..)


type alias Dimension =
    { rows : Int
    , columns : Int
    }


type alias Position =
    { row : Int
    , column : Int
    }



-- Model


type alias Model =
    { dimension : Dimension
    , player : Position
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { dimension = Dimension 10 18
      , player = Position 5 5
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
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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
    positionsByRows dimension
        |> List.map (viewRow playerPosition)


viewRow playerPosition positions =
    div [] (List.map (viewCell playerPosition) positions)


viewCell playerPosition position =
    if position == playerPosition then
        text "3"

    else
        text "."


positionsByRows : { a | rows : Int, columns : Int } -> List (List Position)
positionsByRows { rows, columns } =
    rangeLen rows
        |> List.map
            (\r ->
                rangeLen columns
                    |> List.map (\c -> Position r c)
            )


rangeLen : Int -> List Int
rangeLen len =
    List.range 0 (len - 1)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
