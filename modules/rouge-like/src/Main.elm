module Main exposing (main)

import AStarSearch
import Basics.Extra exposing (..)
import Basics.More exposing (..)
import Browser
import Browser.Events
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



-- UID


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


enemiesRemove : Uid -> List Enemy -> List Enemy
enemiesRemove uid =
    List.filterNot (enemyIdEq uid)


enemiesUpdate : Uid -> (Enemy -> Enemy) -> List Enemy -> List Enemy
enemiesUpdate id =
    List.updateIf (enemyIdEq id)


enemiesFind : Uid -> List Enemy -> Maybe Enemy
enemiesFind uid enemies =
    List.find (enemyIdEq uid) enemies



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
    shuffleSplit 8 acc.empty
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


mapEnemies : (List Enemy -> List Enemy) -> Model -> Model
mapEnemies f model =
    { model | enemies = f model.enemies }


isInvalidOrWall : Location -> Model -> Bool
isInvalidOrWall l model =
    not (Dimension.containsLocation l model.dimension)
        || List.member l model.walls


pathFromTo : Location -> Location -> Model -> List Location
pathFromTo from to model =
    let
        neighbours : Int2 -> List ( Int2, Float )
        neighbours pt =
            Location.fromTuple pt
                |> Location.adjacent
                |> List.filterMap
                    (\location ->
                        if isInvalidOrWall location model then
                            Nothing

                        else
                            Just ( Location.toTuple location, 1 )
                    )

        start : Int2
        start =
            Location.toTuple from

        end : Int2
        end =
            Location.toTuple to

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
    case computePlayerMove direction model of
        Nothing ->
            Nothing

        Just playerMove ->
            model
                |> movePlayer playerMove
                |> stepEnemies
                |> Just


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


computePlayerMove : Direction -> Model -> Maybe PlayerMove
computePlayerMove direction model =
    let
        location =
            stepLocationInDirection direction model.player
    in
    if isInvalidOrWall location model || model.player == location then
        Nothing

    else
        case List.find (enemyLocationEq location) model.enemies of
            Just enemy ->
                Just (PlayerAttackEnemy enemy)

            Nothing ->
                Just (PlayerSetLocation location)


stepEnemies : Model -> Model
stepEnemies model0 =
    model0.enemies
        |> List.map .uid
        |> List.foldl
            (\uid model ->
                stepEnemy uid model
                    |> Maybe.map (moveEnemy uid)
                    |> Maybe.withDefault model
            )
            model0


stepEnemy : Uid -> Model -> Maybe ( EnemyMove, Model )
stepEnemy uid model =
    computeRandomEnemyMove uid model
        |> always (computeEnemyMoveTowardsPlayer uid model)


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


computeEnemyMoveTowardsPlayer : Uid -> Model -> Maybe ( EnemyMove, Model )
computeEnemyMoveTowardsPlayer uid model =
    enemiesFind uid model.enemies
        |> Maybe.andThen
            (\enemy ->
                case pathFromTo enemy.location model.player model of
                    _ :: el :: _ ->
                        toEnemyMove el model
                            |> Maybe.map (pairTo model)

                    _ ->
                        Nothing
            )


enemyMoveTowardsPlayer : Uid -> Model -> Maybe EnemyMove
enemyMoveTowardsPlayer uid model =
    enemiesFind uid model.enemies
        |> Maybe.andThen
            (\enemy ->
                case pathFromTo enemy.location model.player model of
                    _ :: el :: _ ->
                        toEnemyMove el model

                    _ ->
                        Nothing
            )


computeRandomEnemyMove : Uid -> Model -> Maybe ( EnemyMove, Model )
computeRandomEnemyMove uid model =
    case
        randomEnemyMoveGenerator uid model
    of
        Nothing ->
            Nothing

        Just gen ->
            Random.step gen model.seed
                |> (\( em, seed ) -> ( em, { model | seed = seed } ))
                |> Just


randomEnemyMoveGenerator : Uid -> Model -> Maybe (Generator EnemyMove)
randomEnemyMoveGenerator uid model =
    case plausibleEnemyMoves uid model of
        [] ->
            Nothing

        h :: t ->
            Just (Random.uniform h t)


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
                    |> List.filterMap (\location -> toEnemyMove location model)
            )
        |> Maybe.withDefault []


toEnemyMove : Location -> Model -> Maybe EnemyMove
toEnemyMove location model =
    if isInvalidOrWall location model then
        Nothing

    else if model.player == location then
        Just EnemyAttackPlayer

    else
        case List.find (enemyLocationEq location) model.enemies of
            Just enemy ->
                Just (EnemyAttackEnemy enemy)

            Nothing ->
                Just (EnemySetLocation location)


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
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Elm Rouge" ]
        , div [ class "flex relative" ]
            [ viewGrid model
            , viewOverlay model
            ]
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


type Cell
    = Player
    | Enemy_ Enemy
    | Wall


viewGrid : Model -> HM
viewGrid model =
    let
        grid =
            MGrid.empty model.dimension
                |> MGrid.fill model.walls Wall
                |> MGrid.setAll (List.map (\e -> ( e.location, Enemy_ e )) model.enemies)
                |> MGrid.set model.player Player

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
    div [ class "center code f2 bg-black white pa3 br3" ]
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
