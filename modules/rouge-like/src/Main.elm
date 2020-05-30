module Main exposing (main)

import AStarSearch
import Basics.Extra exposing (..)
import Basics.More exposing (..)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import List.Extra as List
import Position exposing (Position)
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Random.List


type Uid
    = Uid Int


newUid : Generator Uid
newUid =
    Random.int Random.minInt Random.maxInt
        |> Random.map Uid



--


type alias Enemy =
    { uid : Uid
    , position : Position
    }


newEnemy : Position -> Generator Enemy
newEnemy position =
    newUid
        |> Random.map
            (\uid ->
                { uid = uid
                , position = position
                }
            )


enemyPositionEq : Position -> Enemy -> Bool
enemyPositionEq position enemy =
    enemy.position == position


enemySetPosition : Position -> Enemy -> Enemy
enemySetPosition position enemy =
    { enemy | position = position }


enemyIdEq : Uid -> Enemy -> Bool
enemyIdEq uid enemy =
    enemy.uid == uid



-- Enemies


enemiesRemoveAtPosition : Position -> List Enemy -> List Enemy
enemiesRemoveAtPosition position =
    List.filterNot (enemyPositionEq position)


enemiesRemove : Uid -> List Enemy -> List Enemy
enemiesRemove uid =
    List.filterNot (enemyIdEq uid)


enemiesUpdate : Uid -> (Enemy -> Enemy) -> List Enemy -> List Enemy
enemiesUpdate id =
    List.updateIf (enemyIdEq id)


enemiesFind : Uid -> List Enemy -> Maybe Enemy
enemiesFind uid enemies =
    List.find (enemyIdEq uid) enemies


enemiesFindAtPosition : Position -> List Enemy -> Maybe Enemy
enemiesFindAtPosition position =
    List.find (enemyPositionEq position)



-- World Generator


type alias WorldBuilder =
    { empty : List Position
    , player : Position
    , walls : List Position
    , enemies : List Enemy
    }


newWorldBuilder : Dimension -> Generator WorldBuilder
newWorldBuilder dimension =
    let
        acc : WorldBuilder
        acc =
            { empty =
                dimension
                    |> Dimension.toPositions
                    |> List.remove playerPosition
            , player = playerPosition
            , walls = []
            , enemies = []
            }

        playerPosition =
            Position.new 5 5
    in
    wallsGenerator acc
        |> Random.andThen enemiesGenerator


enemiesGenerator : WorldBuilder -> Generator WorldBuilder
enemiesGenerator acc =
    shuffleSplit 18 acc.empty
        |> Random.andThen
            (\( enemyPositions, empty ) ->
                enemyPositions
                    |> List.map newEnemy
                    |> Random.combine
                    |> Random.map
                        (\enemies ->
                            { acc | enemies = enemies, empty = empty }
                        )
            )


wallsGenerator : WorldBuilder -> Generator WorldBuilder
wallsGenerator acc =
    shuffleSplit 20 acc.empty
        |> Random.map
            (\( walls, empty ) ->
                { acc | walls = walls, empty = empty }
            )


shuffleSplit : Int -> List a -> Generator ( List a, List a )
shuffleSplit n xs =
    Random.List.shuffle xs
        |> Random.andThen Random.List.shuffle
        |> Random.map (List.splitAt n)



-- Model


type alias Model =
    { dimension : Dimension
    , player : Position
    , playerHp : Int
    , walls : List Position
    , enemies : List Enemy
    , seed : Seed
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 10 18

        ( acc, seed ) =
            Random.step (newWorldBuilder dimension) (Random.initialSeed flags.now)
    in
    ( { dimension = dimension
      , player = acc.player
      , playerHp = 3
      , walls = acc.walls
      , enemies = acc.enemies
      , seed = seed
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
            ( if model.playerHp > 0 then
                directionFromKey key
                    |> Maybe.andThen (\direction -> stepPlayerInDirection direction model)
                    |> Maybe.withDefault model

              else
                model
            , Cmd.none
            )


stepPlayerInDirection : Direction -> Model -> Maybe Model
stepPlayerInDirection direction model =
    model
        |> computePlayerMove direction (toGrid model)
        |> Maybe.map
            ((\playerMove -> movePlayer playerMove model)
                >> stepEnemies
            )


movePlayer : PlayerMove -> Model -> Model
movePlayer playerMove model =
    case playerMove of
        PlayerSetPosition position ->
            { model | player = position }

        PlayerAttackEnemy enemy ->
            { model | player = enemy.position }
                |> mapEnemies (enemiesRemove enemy.uid)


type PlayerMove
    = PlayerSetPosition Position
    | PlayerAttackEnemy Enemy


computePlayerMove : Direction -> Grid.Grid Entity -> Model -> Maybe PlayerMove
computePlayerMove direction grid model =
    let
        position =
            stepPositionInDirection direction model.player
    in
    grid
        |> Grid.slotAt position
        |> Maybe.andThen
            (\slot ->
                case slot of
                    Grid.Filled entity ->
                        case entity of
                            Player ->
                                Nothing

                            Enemy_ enemy ->
                                Just (PlayerAttackEnemy enemy)

                            Wall ->
                                Nothing

                    Grid.Empty ->
                        Just (PlayerSetPosition position)
            )


mapEnemies : (List Enemy -> List Enemy) -> Model -> Model
mapEnemies f model =
    { model | enemies = f model.enemies }


stepEnemies : Model -> Model
stepEnemies model =
    model.enemies
        |> List.map .uid
        |> List.foldl (ignoreNothing2 stepEnemy) model


stepEnemy : Uid -> Model -> Maybe Model
stepEnemy uid model =
    computeEnemyMove uid model
        |> Maybe.map (moveEnemy uid)


moveEnemy : Uid -> ( EnemyMove, Model ) -> Model
moveEnemy uid ( enemyMove, model ) =
    case enemyMove of
        EnemyAttackPlayer ->
            { model
                | playerHp =
                    model.playerHp - 1 |> atLeast 0
            }
                |> mapEnemies (enemiesRemove uid)

        EnemyAttackEnemy victim ->
            model
                |> mapEnemies (enemiesRemove victim.uid)

        EnemySetPosition position ->
            model
                |> mapEnemies (enemiesUpdate uid (enemySetPosition position))


type Entity
    = Player
    | Enemy_ Enemy
    | Wall



--classifyPosition : Model -> Position -> Maybe Entity
--classifyPosition model position =
--    if Dimension.containsPosition position model.dimension then
--        Just
--            -- Is Player
--            (if model.player == position then
--                Player
--                -- Is Wall
--
--             else if List.member position model.walls then
--                Wall
--
--             else
--                case enemiesFindAtPosition position model.enemies of
--                    Nothing ->
--                        Empty
--
--                    Just enemy ->
--                        Enemy_ enemy
--            )
--
--    else
--        Nothing


computeEnemyMove : Uid -> Model -> Maybe ( EnemyMove, Model )
computeEnemyMove uid model =
    case
        plausibleEnemyMoves uid model
            |> Random.sample
            |> (\g -> Random.step g model.seed)
    of
        ( Nothing, _ ) ->
            Nothing

        ( Just enemyMove, seed ) ->
            Just ( enemyMove, { model | seed = seed } )


type EnemyMove
    = EnemySetPosition Position
    | EnemyAttackPlayer
    | EnemyAttackEnemy Enemy


plausibleEnemyMoves : Uid -> Model -> List EnemyMove
plausibleEnemyMoves uid model =
    enemiesFind uid model.enemies
        |> Maybe.map
            (\enemy ->
                Position.adjacent enemy.position
                    |> List.filterMap (flip toEnemyMove (toGrid model))
            )
        |> Maybe.withDefault []


toEnemyMove : Position -> Grid.Grid Entity -> Maybe EnemyMove
toEnemyMove position grid =
    Grid.slotAt position grid
        |> Maybe.andThen
            (\slot ->
                case slot of
                    Grid.Filled entity ->
                        case entity of
                            Player ->
                                Just EnemyAttackPlayer

                            Enemy_ e ->
                                Just (EnemyAttackEnemy e)

                            Wall ->
                                Nothing

                    Grid.Empty ->
                        Just (EnemySetPosition position)
            )


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
    let
        grid =
            toGrid model
    in
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Elm Rouge" ]
        , div [ class "flex relative" ]
            [ viewGrid grid model
            , viewOverlay model
            ]
        , div [ class "pv3 f3" ] [ text "A* Path finding Debug" ]
        , viewPathGrid model
        ]


type alias Int2 =
    ( Int, Int )


toGrid : Model -> Grid.Grid Entity
toGrid model =
    Grid.empty model.dimension
        |> Grid.fill model.walls Wall
        |> Grid.setAll (List.map (\e -> ( e.position, Enemy_ e )) model.enemies)
        |> Grid.set model.player Player


viewPathGrid : Model -> HM
viewPathGrid model =
    let
        grid =
            toGrid model

        neighbours : Int2 -> List ( Int2, Float )
        neighbours pt =
            grid
                |> Grid.adjacent (Position.fromTuple pt)
                |> List.filterMap
                    (\( position, slot ) ->
                        case slot of
                            Grid.Filled entity ->
                                case entity of
                                    Player ->
                                        Just position

                                    Enemy_ _ ->
                                        Just position

                                    Wall ->
                                        Nothing

                            Grid.Empty ->
                                Just position
                    )
                |> List.map (\p -> ( Position.toTuple p, 1 ))

        start : Int2
        start =
            Position.new 0 0
                |> Position.toTuple

        end : Int2
        end =
            model.dimension
                |> Dimension.maxPosition
                --|> always (Position.new 1 1)
                |> Position.toTuple

        cost : Int2 -> Float
        cost ( row, column ) =
            let
                ( er, ec ) =
                    end
            in
            (abs (row - er) + abs (column - ec))
                |> toFloat

        path : List Position
        path =
            AStarSearch.aStar neighbours cost start end
                |> List.map Position.fromTuple
                |> Debug.log "debug"

        isInPath p =
            List.member p path
    in
    div [ class "flex items-center justify-center" ]
        [ div [ class "debug-white" ]
            (Grid.toEntryRows grid
                |> List.map
                    (\rowEntries ->
                        div [ class "flex" ]
                            (rowEntries
                                |> List.map
                                    (\( position, slot ) ->
                                        div
                                            [ class "w1 h1"
                                            , case slot of
                                                Grid.Filled entity ->
                                                    case entity of
                                                        Player ->
                                                            class "bg-blue"

                                                        Enemy_ _ ->
                                                            class "bg-red"

                                                        Wall ->
                                                            class "bg-gray"

                                                Grid.Empty ->
                                                    if isInPath position then
                                                        class "bg-green"

                                                    else
                                                        class "bg-white"
                                            ]
                                            []
                                    )
                            )
                    )
            )
        ]


viewOverlay : Model -> HM
viewOverlay model =
    let
        alive =
            model.playerHp > 0

        allEnemiesDead =
            List.length model.enemies == 0

        won =
            alive && allEnemiesDead

        lost =
            not alive

        over =
            won || lost

        subTitle =
            if won then
                "You Won!"

            else if lost then
                "You Lost!"

            else
                ""
    in
    if over then
        div
            [ class "absolute w-100 h-100 flex items-center justify-center"
            ]
            [ div [ class "bg-white-50 black pa3 br3" ]
                [ div [ class "f2 tc" ] [ text subTitle ]
                , div [ class "f3 tc" ] [ text "Ctrl+R to restart" ]
                ]
            ]

    else
        text ""


type alias HM =
    Html Msg


viewGrid : Grid.Grid Entity -> Model -> HM
viewGrid grid model =
    let
        slotToChar slot =
            case slot of
                Grid.Filled entity ->
                    case entity of
                        Player ->
                            String.fromInt (abs model.playerHp)
                                |> String.toList
                                |> headOr '3'

                        Enemy_ _ ->
                            'e'

                        Wall ->
                            '#'

                Grid.Empty ->
                    '.'
    in
    div [ class "code f2 bg-black white pa3 br3" ]
        (grid
            |> Grid.toRows
            |> List.map
                (List.map
                    (slotToChar
                        >> String.fromChar
                        >> text
                    )
                    >> div []
                )
        )



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
