module Main exposing (main)

import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as JD
import List.Extra as List
import Maybe.Extra exposing (unwrap)
import Position exposing (Position)
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Random.List
import Tuple exposing (first, second)


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


atLeast =
    max


enemyPositionEq : Position -> Enemy -> Bool
enemyPositionEq position enemy =
    enemy.position == position


enemySetPosition : Position -> Enemy -> Enemy
enemySetPosition position enemy =
    { enemy | position = position }



-- Enemies


enemiesRemoveAtPosition : Position -> List Enemy -> List Enemy
enemiesRemoveAtPosition position =
    List.filter (enemyPositionEq position >> not)



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
                            { acc
                                | enemies = enemies
                                , empty = empty
                            }
                        )
            )


wallsGenerator : WorldBuilder -> Generator WorldBuilder
wallsGenerator acc =
    shuffleSplit 20 acc.empty
        |> Random.map
            (\( walls, empty ) ->
                { acc
                    | walls = walls
                    , empty = empty
                }
            )



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


shuffleSplit : Int -> List a -> Generator ( List a, List a )
shuffleSplit n xs =
    Random.List.shuffle xs
        |> Random.andThen Random.List.shuffle
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

        allTrue =
            List.all (\f -> f model position)
    in
    if
        allTrue
            [ isWithinDimension
            , notWall
            ]
    then
        { model | player = position }
            |> mapEnemies (enemiesRemoveAtPosition position)
            |> generate stepEnemiesGenerator

    else
        model


mapEnemies : (List Enemy -> List Enemy) -> Model -> Model
mapEnemies f model =
    { model | enemies = f model.enemies }


generate : ({ a | seed : Seed } -> Generator { b | seed : Seed }) -> { a | seed : Seed } -> { b | seed : Seed }
generate f model =
    Random.step (f model) model.seed
        |> setSeedIn


setSeedIn ( model, seed ) =
    { model | seed = seed }


stepEnemiesGenerator : Model -> Generator Model
stepEnemiesGenerator model =
    model.enemies
        |> List.map .uid
        |> List.foldl (\uid -> Random.andThen (stepEnemyWithUid uid)) (Random.constant model)


enemyIdEq : Uid -> Enemy -> Bool
enemyIdEq uid enemy =
    enemy.uid == uid


stepEnemyWithUid : Uid -> Model -> Generator Model
stepEnemyWithUid uid model =
    case List.find (enemyIdEq uid) model.enemies of
        Nothing ->
            Random.constant model

        Just enemy ->
            nextEnemyPositionGenerator model enemy
                |> Random.map
                    (\mp ->
                        case mp of
                            Nothing ->
                                model

                            Just nextPosition ->
                                moveEnemyTo nextPosition enemy model
                    )


moveEnemyTo : Position -> Enemy -> Model -> Model
moveEnemyTo nextPosition enemy model =
    case entityAt model nextPosition of
        E_Player ->
            { model
                | enemies = model.enemies |> List.remove enemy
                , playerHp = model.playerHp - 1 |> atLeast 0
            }

        E_Enemy victim ->
            { model | enemies = model.enemies |> List.remove victim }

        E_Wall ->
            model

        E_Empty ->
            { model
                | enemies =
                    model.enemies
                        |> List.updateIf ((==) enemy) (enemySetPosition nextPosition)
            }


type Entity
    = E_Player
    | E_Enemy Enemy
    | E_Wall
    | E_Empty


entityAt : Model -> Position -> Entity
entityAt model position =
    if isPlayer model position then
        E_Player

    else if isWall model position then
        E_Wall

    else
        case List.find (enemyPositionEq position) model.enemies of
            Nothing ->
                E_Empty

            Just enemy ->
                E_Enemy enemy


enemyMoves : Model -> Enemy -> List Position
enemyMoves model enemy =
    Dimension.adjacentPositions enemy.position model.dimension
        |> List.filter (notWall model)


nextEnemyPositionGenerator : Model -> Enemy -> Generator (Maybe Position)
nextEnemyPositionGenerator model enemy =
    case enemyMoves model enemy of
        [] ->
            Random.constant Nothing

        h :: t ->
            Random.maybe Random.bool (Random.uniform h t)


isWithinDimension : Model -> Position -> Bool
isWithinDimension model position =
    Dimension.member position model.dimension


isWall : Model -> Position -> Bool
isWall model position =
    List.member position model.walls


isEnemy : Model -> Position -> Bool
isEnemy model position =
    List.find (enemyPositionEq position) model.enemies
        |> unwrap False (always True)


isPlayer : Model -> Position -> Bool
isPlayer model position =
    model.player == position


notWall : Model -> Position -> Bool
notWall model =
    isWall model >> not


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
    [ ( isPlayer
      , String.fromInt model.playerHp
            |> String.toList
            |> List.last
            |> Maybe.withDefault '3'
      )
    , ( isEnemy, 'e' )
    , ( isWall, '#' )
    ]
        |> List.find (first >> (\f -> f model position))
        |> unwrap '.' second



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
