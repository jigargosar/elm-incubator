module Main exposing (main)

import AStarSearch
import Basics.Extra exposing (..)
import Basics.More exposing (..)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import List.Extra as List
import Location exposing (Location)
import MGrid
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
    , location : Location
    }


newEnemy : Location -> Generator Enemy
newEnemy location =
    newUid
        |> Random.map
            (\uid ->
                { uid = uid
                , location = location
                }
            )


enemyLocationEq : Location -> Enemy -> Bool
enemyLocationEq location enemy =
    enemy.location == location


enemySetLocation : Location -> Enemy -> Enemy
enemySetLocation location enemy =
    { enemy | location = location }


enemyIdEq : Uid -> Enemy -> Bool
enemyIdEq uid enemy =
    enemy.uid == uid



-- Enemies


enemiesRemoveAtLocation : Location -> List Enemy -> List Enemy
enemiesRemoveAtLocation location =
    List.filterNot (enemyLocationEq location)


enemiesRemove : Uid -> List Enemy -> List Enemy
enemiesRemove uid =
    List.filterNot (enemyIdEq uid)


enemiesUpdate : Uid -> (Enemy -> Enemy) -> List Enemy -> List Enemy
enemiesUpdate id =
    List.updateIf (enemyIdEq id)


enemiesFind : Uid -> List Enemy -> Maybe Enemy
enemiesFind uid enemies =
    List.find (enemyIdEq uid) enemies


enemiesFindAtLocation : Location -> List Enemy -> Maybe Enemy
enemiesFindAtLocation location =
    List.find (enemyLocationEq location)



-- World Generator


type alias WorldBuilder =
    { empty : List Location
    , player : Location
    , walls : List Location
    , enemies : List Enemy
    }


newWorldBuilder : Dimension -> Generator WorldBuilder
newWorldBuilder dimension =
    let
        acc : WorldBuilder
        acc =
            { empty =
                dimension
                    |> Dimension.toLocations
                    |> List.remove playerLocation
            , player = playerLocation
            , walls = []
            , enemies = []
            }

        playerLocation =
            Location.new 5 5
    in
    wallsGenerator acc
        |> Random.andThen enemiesGenerator


enemiesGenerator : WorldBuilder -> Generator WorldBuilder
enemiesGenerator acc =
    shuffleSplit 18 acc.empty
        |> Random.andThen
            (\( enemyLocations, empty ) ->
                enemyLocations
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
    , player : Location
    , playerHp : Int
    , walls : List Location
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
        PlayerSetLocation location ->
            { model | player = location }

        PlayerAttackEnemy enemy ->
            { model | player = enemy.location }
                |> mapEnemies (enemiesRemove enemy.uid)


type PlayerMove
    = PlayerSetLocation Location
    | PlayerAttackEnemy Enemy


computePlayerMove : Direction -> MGrid.MGrid Entity -> Model -> Maybe PlayerMove
computePlayerMove direction grid model =
    let
        location =
            stepLocationInDirection direction model.player
    in
    grid
        |> MGrid.maybeFilledAt location
        |> Maybe.andThen
            (\maybeFilled ->
                case maybeFilled of
                    Just entity ->
                        case entity of
                            Player ->
                                Nothing

                            Enemy_ enemy ->
                                Just (PlayerAttackEnemy enemy)

                            Wall ->
                                Nothing

                    Nothing ->
                        Just (PlayerSetLocation location)
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

        EnemySetLocation location ->
            model
                |> mapEnemies (enemiesUpdate uid (enemySetLocation location))


type Entity
    = Player
    | Enemy_ Enemy
    | Wall


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
    = EnemySetLocation Location
    | EnemyAttackPlayer
    | EnemyAttackEnemy Enemy


plausibleEnemyMoves : Uid -> Model -> List EnemyMove
plausibleEnemyMoves uid model =
    enemiesFind uid model.enemies
        |> Maybe.map
            (\enemy ->
                Location.adjacent enemy.location
                    |> List.filterMap (flip toEnemyMove (toGrid model))
            )
        |> Maybe.withDefault []


toEnemyMove : Location -> MGrid.MGrid Entity -> Maybe EnemyMove
toEnemyMove location grid =
    MGrid.maybeFilledAt location grid
        |> Maybe.andThen
            (\maybeFilled ->
                case maybeFilled of
                    Just entity ->
                        case entity of
                            Player ->
                                Just EnemyAttackPlayer

                            Enemy_ e ->
                                Just (EnemyAttackEnemy e)

                            Wall ->
                                Nothing

                    Nothing ->
                        Just (EnemySetLocation location)
            )


stepLocationInDirection : Direction -> Location -> Location
stepLocationInDirection direction =
    case direction of
        Left ->
            Location.left

        Right ->
            Location.right

        Up ->
            Location.up

        Down ->
            Location.down


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
        , viewPathGrid grid
        ]


type alias Int2 =
    ( Int, Int )


toGrid : Model -> MGrid.MGrid Entity
toGrid model =
    MGrid.empty model.dimension
        |> MGrid.fill model.walls Wall
        |> MGrid.setAll (List.map (\e -> ( e.location, Enemy_ e )) model.enemies)
        |> MGrid.set model.player Player


pathFromGrid : MGrid.MGrid Entity -> List Location
pathFromGrid grid =
    let
        neighbours : Int2 -> List ( Int2, Float )
        neighbours pt =
            grid
                |> MGrid.adjacent (Location.fromTuple pt)
                |> List.filterMap
                    (\( location, slot ) ->
                        case slot of
                            MGrid.Filled Wall ->
                                Nothing

                            _ ->
                                Just location
                    )
                |> List.map (\l -> ( Location.toTuple l, 1 ))

        neighbours2 : Int2 -> List ( Int2, Float )
        neighbours2 pt =
            grid
                |> MGrid.adjacent2 pt
                |> List.filterMap
                    (\( location, slot ) ->
                        case slot of
                            MGrid.Filled Wall ->
                                Nothing

                            _ ->
                                Just ( location, 1 )
                    )

        start : Int2
        start =
            Location.new 0 0
                |> Location.toTuple

        end : Int2
        end =
            MGrid.dimension grid
                |> Dimension.maxLocation
                --|> always (Location.new 1 1)
                |> Location.toTuple

        cost : Int2 -> Float
        cost ( row, column ) =
            let
                ( er, ec ) =
                    end
            in
            (abs (row - er) + abs (column - ec))
                |> toFloat
    in
    AStarSearch.aStar neighbours cost start end
        |> List.map Location.fromTuple


viewPathGrid : MGrid.MGrid Entity -> Html msg
viewPathGrid grid =
    let
        path : List Location
        path =
            pathFromGrid grid

        attrGrid =
            grid
                |> MGrid.map
                    (\_ entity ->
                        case entity of
                            Player ->
                                class "bg-blue"

                            Enemy_ _ ->
                                class "bg-red"

                            Wall ->
                                class "bg-gray"
                    )
                |> MGrid.fillWhenEmpty path (class "bg-green")
    in
    div [ class "flex items-center justify-center" ]
        [ div [ class "debug-white" ]
            (attrGrid
                |> MGrid.viewRows (\_ -> div [ class "flex" ])
                    (\_ slot ->
                        div
                            [ class "w1 h1"
                            , case slot of
                                MGrid.Filled attr ->
                                    attr

                                MGrid.Empty ->
                                    class "bg-white"
                            ]
                            []
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


viewGrid : MGrid.MGrid Entity -> Model -> HM
viewGrid grid model =
    let
        slotToChar slot =
            case slot of
                MGrid.Filled entity ->
                    case entity of
                        Player ->
                            String.fromInt (abs model.playerHp)
                                |> String.toList
                                |> headOr '3'

                        Enemy_ _ ->
                            'e'

                        Wall ->
                            '#'

                MGrid.Empty ->
                    '.'
    in
    div [ class "code f2 bg-black white pa3 br3" ]
        (grid
            |> MGrid.viewRows (\_ -> div [])
                (\_ slot ->
                    slot
                        |> slotToChar
                        |> String.fromChar
                        |> text
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
