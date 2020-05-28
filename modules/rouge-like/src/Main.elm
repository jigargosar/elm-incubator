module Main exposing (main)

import Basics.Extra exposing (..)
import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
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
    shuffleSplit 8 acc.empty
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
            ( directionFromKey key
                |> Maybe.andThen (\direction -> stepPlayerInDirection direction model)
                |> Maybe.withDefault model
            , Cmd.none
            )


stepPlayerInDirection : Direction -> Model -> Maybe Model
stepPlayerInDirection direction model =
    model
        |> computePlayerMove direction
        |> Maybe.map
            (\position ->
                { model | player = position }
                    |> mapEnemies (enemiesRemoveAtPosition position)
                    |> stepEnemies
            )


computePlayerMove : Direction -> Model -> Maybe Position
computePlayerMove direction model =
    let
        position =
            stepPositionInDirection direction model.player
    in
    if canPlayerMoveTo position model then
        Just position

    else
        Nothing


canPlayerMoveTo : Position -> Model -> Bool
canPlayerMoveTo position model =
    Dimension.member position model.dimension
        && (isWall model position |> not)


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
        |> Maybe.andThen (moveEnemy uid)


moveEnemy : Uid -> ( Position, Model ) -> Maybe Model
moveEnemy uid ( nextPosition, model ) =
    case classifyPosition model nextPosition of
        Nothing ->
            Nothing

        Just entity ->
            (case entity of
                Player ->
                    { model | playerHp = model.playerHp - 1 |> atLeast 0 }
                        |> mapEnemies (enemiesRemove uid)

                Enemy_ victim ->
                    model
                        |> mapEnemies (enemiesRemove victim.uid)

                Wall ->
                    model

                Empty ->
                    model
                        |> mapEnemies (enemiesUpdate uid (enemySetPosition nextPosition))
            )
                |> Just


type Entity
    = Player
    | Enemy_ Enemy
    | Wall
    | Empty


classifyPosition : Model -> Position -> Maybe Entity
classifyPosition model position =
    if Dimension.member position model.dimension then
        Just
            -- Is Player
            (if model.player == position then
                Player
                -- Is Wall

             else if List.member position model.walls then
                Wall

             else
                case enemiesFindAtPosition position model.enemies of
                    Nothing ->
                        Empty

                    Just enemy ->
                        Enemy_ enemy
            )

    else
        Nothing


computeEnemyMove : Uid -> Model -> Maybe ( Position, Model )
computeEnemyMove uid model =
    case
        movesOfEnemy uid model
            |> Random.sample
            |> (\g -> Random.step g model.seed)
    of
        ( Nothing, _ ) ->
            Nothing

        ( Just nextPosition, seed ) ->
            Just ( nextPosition, { model | seed = seed } )


movesOfEnemy : Uid -> Model -> List Position
movesOfEnemy uid model =
    enemiesFind uid model.enemies
        |> Maybe.map (movesOfEnemyHelp model)
        |> Maybe.withDefault []


movesOfEnemyHelp : Model -> Enemy -> List Position
movesOfEnemyHelp model enemy =
    Dimension.adjacentPositions enemy.position model.dimension
        |> List.filterNot (isWall model)


isWall : Model -> Position -> Bool
isWall model position =
    classifyPosition model position == Just Wall


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
        entityToChar e =
            case e of
                Player ->
                    String.fromInt (abs model.playerHp)
                        |> String.toList
                        |> headOr '3'

                Enemy_ _ ->
                    'e'

                Wall ->
                    '#'

                Empty ->
                    '.'

        viewRow entities =
            div []
                [ entities
                    |> List.map entityToChar
                    |> String.fromList
                    |> text
                ]
    in
    div [ class "code f1" ]
        (Dimension.toRows model.dimension
            |> List.map (List.filterMap (classifyPosition model) >> viewRow)
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
