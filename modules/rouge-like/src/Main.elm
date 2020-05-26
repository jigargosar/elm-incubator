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


type alias Enemy =
    { position : Position
    , hp : Int
    }


newEnemy : Position -> Enemy
newEnemy position =
    { position = position
    , hp = 1
    }


atLeast =
    max


enemyTakeHit : Enemy -> Enemy
enemyTakeHit enemy =
    { enemy | hp = enemy.hp - 1 |> atLeast 0 }


enemyPositionEq : Position -> Enemy -> Bool
enemyPositionEq position enemy =
    enemy.position == position


enemySetPosition : Position -> Enemy -> Enemy
enemySetPosition position enemy =
    { enemy | position = position }



-- Model


type alias Model =
    { dimension : Dimension
    , player : Position
    , walls : List Position
    , enemies : List Enemy
    , seed : Seed
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
            Random.initialSeed 10

        ( ( walls, emptyPositions1 ), seed1 ) =
            Random.step (shuffleSplit 20 emptyPositions0) seed0

        ( enemyPositions, _ ) =
            List.splitAt 8 emptyPositions1
    in
    ( { dimension = dimension
      , player = playerPosition
      , walls = walls
      , enemies = List.map newEnemy enemyPositions
      , seed = seed1
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
        { model
            | player = position
            , enemies = List.updateIf (enemyPositionEq position) enemyTakeHit model.enemies
        }
            |> stepEnemies

    else
        model


stepEnemies : Model -> Model
stepEnemies model =
    generate stepEnemiesGenerator model


generate : ({ a | seed : Seed } -> Generator { b | seed : Seed }) -> { a | seed : Seed } -> { b | seed : Seed }
generate f model =
    Random.step (f model) model.seed
        |> setSeedIn


setSeedIn ( model, seed ) =
    { model | seed = seed }


indicesOf : List a -> List Int
indicesOf xs =
    List.range 0 (List.length xs - 1)


stepEnemiesGenerator : Model -> Generator Model
stepEnemiesGenerator model =
    indicesOf model.enemies
        |> List.foldl (\i -> Random.andThen (stepEnemyAtIndex i)) (Random.constant model)


stepEnemyAtIndex : Int -> Model -> Generator Model
stepEnemyAtIndex enemyIndex model =
    case List.getAt enemyIndex model.enemies of
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
                                { model
                                    | enemies =
                                        model.enemies
                                            |> List.updateIf (enemyPositionEq nextPosition) enemyTakeHit
                                            |> List.updateIf ((==) enemy) (enemySetPosition nextPosition)
                                }
                    )


enemyMoves : Model -> Enemy -> List Position
enemyMoves model enemy =
    if enemy.hp > 0 then
        Dimension.adjacentPositions enemy.position model.dimension
            |> List.filter (notWall model)

    else
        []


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
    case List.find (enemyPositionEq position) model.enemies of
        Just enemy ->
            enemy.hp > 0

        Nothing ->
            False


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
    [ ( isPlayer, '3' )
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
