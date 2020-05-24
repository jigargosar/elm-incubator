module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import Tuple exposing (..)



-- Grid


type Tile
    = Player
    | Enemy
    | Wall
    | Empty


tileFromChar : Char -> Tile
tileFromChar char =
    case char of
        '#' ->
            Wall

        '3' ->
            Player

        'e' ->
            Enemy

        _ ->
            Empty


tileToChar : Tile -> Char
tileToChar tile =
    case tile of
        Player ->
            '3'

        Enemy ->
            'e'

        Wall ->
            '#'

        Empty ->
            '.'


type Grid
    = Grid (Dict ( Int, Int ) Tile)


gridInit : Grid
gridInit =
    [ "#..."
    , ".#.e"
    , "e..."
    , "...3"
    ]
        |> List.map (String.toList >> List.map tileFromChar)
        |> List.indexedMap (\r -> List.indexedMap (\c -> pair ( r, c )))
        |> List.concat
        |> Dict.fromList
        |> Grid


gridToRows : Grid -> List (List Tile)
gridToRows (Grid d) =
    Dict.toList d
        |> groupByRow


groupByRow : List ( ( Int, Int ), a ) -> List (List a)
groupByRow =
    let
        reducer ( ( r, c ), a ) =
            Dict.update r
                (Maybe.map (Dict.insert c a)
                    >> Maybe.withDefault (Dict.singleton c a)
                    >> Just
                )
    in
    List.foldl reducer Dict.empty
        >> Dict.values
        >> List.map Dict.values


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
                |> gridToRows
                |> List.map (List.map tileToChar >> String.fromList)
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
