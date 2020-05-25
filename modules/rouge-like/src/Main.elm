module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import Random exposing (Generator)
import Set exposing (Set)
import Tuple exposing (..)



-- Tile


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



-- Grid


type alias TileDict =
    Dict ( Int, Int ) Tile



--type alias GridRecord =
--    { player : ( Int, Int )
--    , enemies : Set ( Int, Int )
--    , walls : Set ( Int, Int )
--    , empty : Set ( Int, Int )
--    }
--
--
--toGridRecord : GridDict -> GridRecord
--toGridRecord =
--    let
--        reducer ( position, t ) gr =
--            case t of
--                Player ->
--                    { gr | player = position }
--
--                Enemy ->
--                    { gr | enemies = Set.insert position gr.enemies }
--
--                Wall ->
--                    { gr | walls = Set.insert position gr.walls }
--
--                Empty ->
--                    { gr | empty = Set.insert position gr.empty }
--    in
--    Dict.toList
--        >> List.foldl reducer
--            { player = ( 0, 0 )
--            , enemies = Set.empty
--            , walls = Set.empty
--            , empty = Set.empty
--            }
--
--
--fromGridRecord : GridRecord -> GridDict
--fromGridRecord gr =
--    ( gr.player, Player )
--        :: (gr.enemies |> Set.toList |> List.map (pairTo Enemy))
--        ++ (gr.walls |> Set.toList |> List.map (pairTo Wall))
--        ++ (gr.empty |> Set.toList |> List.map (pairTo Empty))
--        |> Dict.fromList
--
--
--pairTo b a =
--    pair a b
--


type Grid
    = Grid TileDict


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


gridMovePlayer : Direction -> Grid -> Grid
gridMovePlayer direction =
    case direction of
        Left ->
            gridMoveBy 0 -1

        Right ->
            gridMoveBy 0 1

        Up ->
            gridMoveBy -1 0

        Down ->
            gridMoveBy 1 0


gridMoveBy : Int -> Int -> Grid -> Grid
gridMoveBy dr dc (Grid d) =
    tileDictMoveBy dr dc d |> Grid


tileDictMoveBy : Int -> Int -> TileDict -> TileDict
tileDictMoveBy dr dc d =
    case
        d
            |> Dict.filter (\_ t -> t == Player)
            |> Dict.keys
    of
        (( r, c ) as position) :: [] ->
            let
                candidatePosition =
                    ( r + dr, c + dc )
            in
            case Dict.get candidatePosition d of
                Nothing ->
                    d

                Just t ->
                    case t of
                        Player ->
                            Debug.todo "impl"

                        Enemy ->
                            Dict.insert candidatePosition Player d
                                |> Dict.insert position Empty

                        Wall ->
                            d

                        Empty ->
                            Dict.insert candidatePosition Player d
                                |> Dict.insert position Empty

        _ ->
            Debug.todo "impl"



-- Position


type alias Position =
    { row : Int
    , column : Int
    }


positionsOf : Int -> Int -> List Position
positionsOf rows columns =
    List.range 0 (rows - 1)
        |> List.map (\r -> List.range 0 (columns - 1) |> List.map (Position r))
        |> List.concat


positionsOfDimension : { a | rows : Int, columns : Int } -> List Position
positionsOfDimension { rows, columns } =
    positionsOf rows columns



-- World


type alias World =
    { rows : Int
    , columns : Int
    , player : Position
    , walls : List Position
    , enemies : List Position
    }


worldInit : World
worldInit =
    { rows = 10
    , columns = 18
    , player = Position 0 0
    , walls = []
    , enemies = []
    }


type alias WorldGeneratorConfig =
    { rows : Int
    , columns : Int
    , walls : Int
    , enemies : Int
    }


worldGenerator : WorldGeneratorConfig -> Generator World
worldGenerator config =
    let
        positions =
            positionsOfDimension config

        wallsGenerator =
            Random.list (List.length positions) (Random.weighted ( 20, True ) [ ( 80, False ) ])
                |> Random.map (List.map2 pair positions >> List.filter second >> List.map first)
    in
    Random.constant worldInit



-- More


randomUniformSelectOne : a -> List a -> Generator ( a, List a )
randomUniformSelectOne x xs =
    Random.uniform x xs
        |> Random.map (\s -> ( s, remove s xs ))


remove : a -> List a -> List a
remove x =
    reject ((==) x)


reject : (a -> Bool) -> List a -> List a
reject p =
    List.filter (p >> not)



-- Model


type alias Model =
    { grid : Grid
    , world : World
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { grid = gridInit
      , world = worldInit
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
                    ( mapGrid (gridMovePlayer direction) model, Cmd.none )

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


mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid f model =
    { model | grid = f model.grid }


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
            (model.grid
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
