module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Ease
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as JD
import List.Extra as List
import Location exposing (Location)
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Random.List
import Tuple.More as Tuple


defaultAnimSpeed =
    ticksToMillis 10


ticksToMillis =
    mul (1000 / 60)



-- Timer


type alias Clock =
    { time : Float
    }


clockZero : Clock
clockZero =
    { time = 0 }


clockStep : Float -> Clock -> Clock
clockStep delta clock =
    { clock | time = clock.time + delta }


clockCurrentTime : Clock -> Float
clockCurrentTime clock =
    clock.time


type alias Timer =
    { startTime : Float
    , duration : Float
    }


timerInit : Clock -> Float -> Timer
timerInit clock duration =
    { startTime = clockCurrentTime clock, duration = duration }


timerIsDone : Clock -> Timer -> Bool
timerIsDone clock timer =
    let
        elapsed =
            clockCurrentTime clock - timer.startTime
    in
    elapsed >= timer.duration


timerReset : Clock -> Timer -> Timer
timerReset clock timer =
    { timer | startTime = clockCurrentTime clock }


timerProgress : Clock -> Timer -> Float
timerProgress clock timer =
    let
        elapsed =
            clockCurrentTime clock - timer.startTime
    in
    clamp 0 timer.duration elapsed / timer.duration


timerPendingProgress : Clock -> Timer -> Float
timerPendingProgress clock timer =
    1 - timerProgress clock timer



-- UID


type Uid
    = Uid Int


newUid : Generator Uid
newUid =
    Random.int Random.minInt Random.maxInt
        |> Random.map Uid


type alias Enemy =
    { id : Uid
    , location : Location
    }


enemyToId : Enemy -> Uid
enemyToId =
    .id


newEnemy : Location -> Generator Enemy
newEnemy location =
    newUid
        |> Random.map
            (\uid ->
                { id = uid
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
    enemy.id == uid



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


enemiesToIds : List Enemy -> List Uid
enemiesToIds =
    List.map .id


enemiesFindAt : Location -> List Enemy -> Maybe Enemy
enemiesFindAt location =
    List.find (enemyLocationEq location)



-- Player


type alias Player =
    { location : Location
    , hp : Int
    }


playerInit : Location -> Player
playerInit location =
    { location = location
    , hp = 3
    }


playerAlive : Player -> Bool
playerAlive player =
    player.hp > 0


playerLocationEq : Location -> Player -> Bool
playerLocationEq location player =
    player.location == location


playerMapLocation : (Location -> Location) -> Player -> Player
playerMapLocation f player =
    { player | location = f player.location }



-- Initial World


type alias WorldInit =
    { empty : List Location
    , player : Player
    , walls : List Location
    , enemies : List Enemy
    }


initialWorldGenerator : Dimension -> Generator WorldInit
initialWorldGenerator dimension =
    let
        acc : Player -> WorldInit
        acc player =
            { empty =
                dimension
                    |> Dimension.toLocations
                    |> List.remove player.location
            , player = player
            , walls = []
            , enemies = []
            }
    in
    playerGenerator dimension
        |> Random.map acc
        |> Random.andThen wallsGenerator
        |> Random.andThen enemiesGenerator


playerGenerator : Dimension -> Generator Player
playerGenerator dimension =
    case Dimension.toLocations dimension of
        [] ->
            playerInit Location.zero
                |> Random.constant

        x :: xs ->
            Random.uniform x xs
                |> Random.map playerInit


enemiesGenerator : WorldInit -> Generator WorldInit
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


wallsGenerator : WorldInit -> Generator WorldInit
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
    , walls : List Location
    , state : State
    , clock : Clock
    , seed : Seed
    }


type State
    = PlayerMovingTo Location Timer Player (List Enemy)
    | WaitingForInput Player (List Enemy)



--mapEnemies : (List Enemy -> List Enemy) -> Model -> Model
--mapEnemies f model =
--    { model | enemies = f model.enemies }
--mapPlayer : (Player -> Player) -> Model -> Model
--mapPlayer f model =
--    { model | player = f model.player }


type alias WorldMap a =
    { a
        | dimension : Dimension
        , walls : List Location
    }


worldIsBlockedAt : Location -> WorldMap a -> Bool
worldIsBlockedAt location worldMap =
    not (Dimension.containsLocation location worldMap.dimension)
        || List.member location worldMap.walls


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        initialSeed =
            Random.initialSeed (flags.now |> always 0)

        ( acc, seed ) =
            Random.step (initialWorldGenerator dimension) initialSeed
    in
    ( { dimension = dimension
      , walls = acc.walls
      , state = WaitingForInput acc.player acc.enemies
      , clock = clockZero
      , seed = seed
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            ( case model.state of
                WaitingForInput player enemies ->
                    case directionFromKey key of
                        Just direction ->
                            let
                                locationInDirection =
                                    stepLocationInDirection direction player.location
                            in
                            case classifyLocation locationInDirection player enemies model of
                                HasNoActor ->
                                    let
                                        movingTimer =
                                            timerInit model.clock defaultAnimSpeed
                                    in
                                    { model | state = PlayerMovingTo locationInDirection movingTimer player enemies }

                                Blocked ->
                                    model

                                HasEnemy enemy ->
                                    model

                                HasPlayer ->
                                    model

                        Nothing ->
                            model

                PlayerMovingTo _ _ _ _ ->
                    model
            , Cmd.none
            )

        Tick delta ->
            ( case updateStateOnTick model.clock model.state of
                Just state ->
                    { model | state = state, clock = clockStep delta model.clock }

                Nothing ->
                    { model | clock = clockStep delta model.clock }
            , Cmd.none
            )


updateStateOnTick : Clock -> State -> Maybe State
updateStateOnTick clock state =
    case state of
        PlayerMovingTo location timer player enemies ->
            if timerIsDone clock timer then
                WaitingForInput (playerMapLocation (always location) player) enemies
                    |> Just

            else
                Nothing

        WaitingForInput _ _ ->
            Nothing


type LocationClass
    = Blocked
    | HasEnemy Enemy
    | HasPlayer
    | HasNoActor


classifyLocation : Location -> Player -> List Enemy -> WorldMap a -> LocationClass
classifyLocation location player enemies worldMap =
    if worldIsBlockedAt location worldMap then
        Blocked

    else if playerLocationEq location player then
        HasPlayer

    else
        case enemiesFindAt location enemies of
            Just enemy ->
                HasEnemy enemy

            Nothing ->
                HasNoActor


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
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , Browser.Events.onAnimationFrameDelta Tick
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
        ( player, enemies ) =
            case model.state of
                PlayerMovingTo _ _ player_ enemies_ ->
                    ( player_, enemies_ )

                WaitingForInput player_ enemies_ ->
                    ( player_, enemies_ )

        alive =
            playerAlive player

        allEnemiesDead =
            List.length enemies == 0

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
                [ div [ class "code f2 tc" ] [ text subTitle ]
                , div [ class "code f3 tc" ] [ text "Ctrl+R to restart" ]
                ]
            ]

    else
        text ""


type alias HM =
    Html Msg


cssTranslate : ( Float, Float ) -> String
cssTranslate dxy =
    dxy
        |> Tuple.map pxFromFloat
        |> Tuple.join ","
        |> paren
        |> append "translate"


cssScale s =
    String.fromFloat s
        |> paren
        |> append "scale"


cssTransform xs =
    style "transform" (String.join " " xs)


commonStyles =
    class "w2 h2 flex items-center justify-center absolute top-0 left-0 bg-black-50"


locationToDXY location =
    Location.toTuple location
        |> Tuple.toFloatScaled 32


viewPlayerTile location hp =
    div
        [ commonStyles, cssTransform [ cssTranslate (locationToDXY location) ] ]
        [ text (String.fromInt hp) ]


viewPlayerTileMovingTo clock to timer location hp =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad (timerProgress clock timer))
    in
    div
        [ commonStyles
        , cssTransform
            [ cssTranslate (locationToDXY location)
            , cssTranslate moveDXY
            ]
        ]
        [ text (String.fromInt hp) ]


viewEnemyTile location =
    div
        [ commonStyles, cssTransform [ cssTranslate (locationToDXY location) ] ]
        [ text "e" ]


viewWallTile location =
    div
        [ commonStyles, cssTransform [ cssTranslate (locationToDXY location) ] ]
        [ text "#" ]


viewFloorTile location =
    div [ commonStyles, cssTransform [ cssTranslate (locationToDXY location) ] ]
        [ text "." ]


viewGrid : Model -> HM
viewGrid model =
    let
        dimension =
            model.dimension
    in
    div [ class "center code f2 bg-black white pa3 br3" ]
        [ div
            [ gridWidthStyle dimension.width
            , gridHeightStyle dimension.height
            , class "relative"
            ]
            ([ backgroundTileViews dimension model.walls
             , case model.state of
                WaitingForInput player enemies ->
                    List.map (\enemy -> viewEnemyTile enemy.location) enemies
                        ++ [ viewPlayerTile player.location player.hp ]

                PlayerMovingTo to timer player enemies ->
                    List.map (\enemy -> viewEnemyTile enemy.location) enemies
                        ++ [ viewPlayerTileMovingTo model.clock to timer player.location player.hp ]
             ]
                |> List.concat
            )
        ]


gridWidthStyle width =
    style "width" (String.fromInt (width * 32) ++ "px")


gridHeightStyle height =
    style "height" (String.fromInt (height * 32) ++ "px")


backgroundTileViews : Dimension -> List Location -> List HM
backgroundTileViews dimension walls =
    Dimension.toLocations dimension
        |> List.map
            (\location ->
                if List.member location walls then
                    viewWallTile location

                else
                    viewFloorTile location
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
