module Main exposing (main)

import Basics.Extra exposing (atLeast)
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


enemyPositionEq : Position -> Enemy -> Bool
enemyPositionEq position enemy =
    enemy.position == position


enemySetPosition : Position -> Enemy -> Enemy
enemySetPosition position enemy =
    { enemy | position = position }



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
            |> stepEnemies

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


stepEnemies : Model -> Model
stepEnemies model =
    model.enemies
        |> List.map .uid
        |> List.foldl (\uid -> generate (stepEnemyWithUid uid)) model


enemyIdEq : Uid -> Enemy -> Bool
enemyIdEq uid enemy =
    enemy.uid == uid


stepEnemyWithUid : Uid -> Model -> Generator Model
stepEnemyWithUid uid model =
    case movesOfEnemyWithId uid model of
        [] ->
            Random.constant model

        h :: t ->
            Random.uniform h t
                |> Random.map
                    (\nextPosition ->
                        case classifyPosition model nextPosition of
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


type Entity
    = Player
    | Enemy_ Enemy
    | Wall
    | Empty


classifyPosition : Model -> Position -> Entity
classifyPosition model position =
    if isPlayer model position then
        Player

    else if isWall model position then
        Wall

    else
        case enemiesFindAtPosition position model.enemies of
            Nothing ->
                Empty

            Just enemy ->
                Enemy_ enemy


movesOfEnemyWithId : Uid -> Model -> List Position
movesOfEnemyWithId uid model =
    enemiesFind uid model.enemies
        |> Maybe.map (enemyMoves model)
        |> Maybe.withDefault []


enemyMoves : Model -> Enemy -> List Position
enemyMoves model enemy =
    Dimension.adjacentPositions enemy.position model.dimension
        |> List.filter (notWall model)


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
