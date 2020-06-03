module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Dimension exposing (Dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as JD
import List.Extra as List
import Location exposing (Location)
import MGrid
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Random.List
import Tuple.More as Tuple



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
    , turn : Turn
    , seed : Seed
    }


type Turn
    = PlayerTurn
    | EnemyTurn EnemyTurnModel


type alias EnemyTurnModel =
    { current : Enemy
    , status : EnemyStatus
    , pendingIds : List Uid
    , counter : Counter
    }


type alias Counter =
    { elapsed : Float
    , target : Float
    }


counterInit : Float -> Counter
counterInit target =
    { elapsed = 0, target = target }


counterProgress : Counter -> Float
counterProgress c =
    clamp 0 c.target c.elapsed / c.target


counterPendingProgress : Counter -> Float
counterPendingProgress c =
    1 - counterProgress c


counterReset : Counter -> Counter
counterReset c =
    { c | elapsed = 0 }


counterTick : Counter -> Counter
counterTick c =
    { c | elapsed = c.elapsed + 1 }


counterIsDone : Counter -> Bool
counterIsDone c =
    c.elapsed >= c.target


type EnemyStatus
    = EnemyStarting
    | EnemyMoving ( Location, Location )
    | EnemyEnding


mapEnemies : (List Enemy -> List Enemy) -> Model -> Model
mapEnemies f model =
    { model | enemies = f model.enemies }


mapPlayerHp : (Int -> Int) -> Model -> Model
mapPlayerHp f model =
    { model | playerHp = f model.playerHp }


isInvalidOrWall : Location -> Model -> Bool
isInvalidOrWall l model =
    not (Dimension.containsLocation l model.dimension)
        || List.member l model.walls


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        ( acc, seed ) =
            Random.step (newWorldBuilder dimension) (Random.initialSeed flags.now)
    in
    ( { dimension = dimension
      , player = acc.player
      , playerHp = 3
      , walls = acc.walls
      , enemies = acc.enemies
      , turn = PlayerTurn
      , seed = seed
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | KeyDown String
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            case model.turn of
                PlayerTurn ->
                    ( if model.playerHp > 0 then
                        toPlayerInput key
                            |> Maybe.andThen (\playerInput -> stepPlayerInput playerInput model)
                            |> Maybe.withDefault model

                      else
                        model
                    , Cmd.none
                    )

                EnemyTurn _ ->
                    ( model, Cmd.none )

        Tick ->
            case model.turn of
                PlayerTurn ->
                    ( model, Cmd.none )

                EnemyTurn etm ->
                    ( if counterIsDone etm.counter then
                        case etm.status of
                            EnemyStarting ->
                                case
                                    computeEnemyMoves etm.current model
                                        |> maybeUniformGenerator
                                of
                                    Nothing ->
                                        model
                                            |> setEnemyTurn (etmSetStatus EnemyEnding etm)

                                    Just emGen ->
                                        let
                                            ( em, seed ) =
                                                Random.step emGen model.seed
                                        in
                                        { model | seed = seed }
                                            |> performEnemyMove etm.current.uid em
                                            |> setEnemyTurn
                                                (etmSetStatus
                                                    (case em of
                                                        EnemySetLocation to ->
                                                            EnemyMoving ( etm.current.location, to )

                                                        _ ->
                                                            EnemyEnding
                                                    )
                                                    etm
                                                )

                            EnemyMoving _ ->
                                model |> setEnemyTurn (etmSetStatus EnemyEnding etm)

                            EnemyEnding ->
                                etmSelectNextEnemy model.enemies etm
                                    |> Maybe.map (flip setEnemyTurn model)
                                    |> Maybe.withDefault { model | turn = PlayerTurn }

                      else
                        model |> setEnemyTurn { etm | counter = counterTick etm.counter }
                    , Cmd.none
                    )


setEnemyTurn : EnemyTurnModel -> Model -> Model
setEnemyTurn etm model =
    { model | turn = EnemyTurn etm }


etmSetStatus : EnemyStatus -> EnemyTurnModel -> EnemyTurnModel
etmSetStatus status etm =
    { etm | status = status, counter = counterReset etm.counter }


etmInit : Enemy -> List Uid -> EnemyTurnModel
etmInit current pendingIds =
    { current = current
    , status = EnemyStarting
    , pendingIds = pendingIds
    , counter = counterInit 40
    }


etmSelectNextEnemy : List Enemy -> EnemyTurnModel -> Maybe EnemyTurnModel
etmSelectNextEnemy enemies etm =
    enemiesFindFirst etm.pendingIds enemies
        |> Maybe.map (\( enemy, pendingIds ) -> etmInit enemy pendingIds)


enemiesFindFirst : List Uid -> List Enemy -> Maybe ( Enemy, List Uid )
enemiesFindFirst uidList enemies =
    case uidList of
        [] ->
            Nothing

        x :: xs ->
            case enemiesFind x enemies of
                Just enemy ->
                    Just ( enemy, xs )

                Nothing ->
                    enemiesFindFirst xs enemies


type PlayerInput
    = StepInDirection Direction
    | StayPut


toPlayerInput : String -> Maybe PlayerInput
toPlayerInput key =
    case key of
        " " ->
            Just StayPut

        _ ->
            directionFromKey key
                |> Maybe.map StepInDirection


stepPlayerInput : PlayerInput -> Model -> Maybe Model
stepPlayerInput playerInput model =
    case playerInput of
        StepInDirection direction ->
            computePlayerMove direction model
                |> Maybe.map
                    (\playerMove ->
                        model
                            |> performPlayerMove playerMove
                            --|> stepEnemies
                            |> initEnemyTurn
                    )

        StayPut ->
            --stepEnemies model |> Just
            initEnemyTurn model |> Just


initEnemyTurn : Model -> Model
initEnemyTurn model =
    case model.enemies |> List.uncons of
        Nothing ->
            model

        Just ( current, pendingEnemies ) ->
            { model
                | turn =
                    EnemyTurn (etmInit current (List.map .uid pendingEnemies))
            }


performPlayerMove : PlayerMove -> Model -> Model
performPlayerMove playerMove model =
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


computeEnemyMoves : Enemy -> Model -> List EnemyMove
computeEnemyMoves enemy model =
    if isPlayerOutOfEnemyRange enemy model then
        plausibleEnemyMoves enemy model

    else
        betterEnemyMovesToWardsPlayer enemy model


isPlayerOutOfEnemyRange : Enemy -> Model -> Bool
isPlayerOutOfEnemyRange enemy model =
    let
        outOfRange diff =
            abs diff >= 5
    in
    Location.map2 sub model.player enemy.location
        |> Location.any outOfRange


performEnemyMove : Uid -> EnemyMove -> Model -> Model
performEnemyMove uid enemyMove model =
    case enemyMove of
        EnemyAttackPlayer ->
            model
                |> mapPlayerHp (dec >> atLeast 0)
                |> mapEnemies (enemiesRemove uid)

        EnemyAttackEnemy victim ->
            model
                |> mapEnemies (enemiesRemove victim.uid)

        EnemySetLocation location ->
            model
                |> mapEnemies (enemiesUpdate uid (enemySetLocation location))


betterEnemyMovesToWardsPlayer : Enemy -> Model -> List EnemyMove
betterEnemyMovesToWardsPlayer enemy model =
    let
        currentDistance =
            Location.manhattanDistance enemy.location model.player

        isBetter newLocation =
            Location.manhattanDistance newLocation model.player < currentDistance
    in
    Location.adjacent enemy.location
        |> List.filterMap
            (\location ->
                if isBetter location then
                    toEnemyMove location model

                else
                    Nothing
            )


type EnemyMove
    = EnemySetLocation Location
    | EnemyAttackPlayer
    | EnemyAttackEnemy Enemy


plausibleEnemyMoves : Enemy -> Model -> List EnemyMove
plausibleEnemyMoves enemy model =
    Location.adjacent enemy.location
        |> List.filterMap (\location -> toEnemyMove location model)


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
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , case model.turn of
            PlayerTurn ->
                Sub.none

            EnemyTurn _ ->
                Browser.Events.onAnimationFrameDelta (always Tick)
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
                [ div [ class "code f2 tc" ] [ text subTitle ]
                , div [ class "code f3 tc" ] [ text "Ctrl+R to restart" ]
                ]
            ]

    else
        text ""


type alias HM =
    Html Msg


type Cell
    = Player Bool Int
    | Enemy_ (Maybe ( EnemyStatus, Counter ))
    | Wall


viewSlot : MGrid.Slot Cell -> HM
viewSlot slot =
    let
        commonStyles =
            class "w2 h2 flex items-center justify-center"
    in
    case slot of
        MGrid.Filled entity ->
            case entity of
                Player isSelected hp ->
                    div
                        [ commonStyles
                        , if isSelected then
                            class "outline"

                          else
                            class ""
                        ]
                        [ text (String.fromInt hp)
                        ]

                Enemy_ maybeES ->
                    let
                        styles =
                            case maybeES of
                                Just ( EnemyMoving fromTo, counter ) ->
                                    let
                                        dxy =
                                            Tuple.map Location.toTuple fromTo
                                                |> uncurry Tuple.sub
                                                |> Tuple.toFloatScaled (32 * counterPendingProgress counter)

                                        ts =
                                            dxy
                                                |> Tuple.map pxFromFloat
                                                |> Tuple.join ","
                                                |> paren
                                                |> append "translate"
                                    in
                                    [ style "transform" ts ]

                                _ ->
                                    []
                    in
                    div [ commonStyles, class "relative" ]
                        [ div
                            ([ commonStyles
                             , class "absolute"
                             , case maybeES of
                                Just _ ->
                                    class "outline"

                                Nothing ->
                                    class ""
                             ]
                                ++ styles
                            )
                            [ text "e" ]
                        ]

                Wall ->
                    div [ commonStyles ] [ text "#" ]

        MGrid.Empty ->
            div [ commonStyles ] [ text "." ]


viewGrid : Model -> HM
viewGrid model =
    let
        isPlayerSelected =
            case model.turn of
                PlayerTurn ->
                    True

                EnemyTurn _ ->
                    False

        toETMStatus uid =
            case model.turn of
                PlayerTurn ->
                    Nothing

                EnemyTurn et ->
                    if et.current.uid == uid then
                        Just ( et.status, et.counter )

                    else
                        Nothing

        grid =
            MGrid.empty model.dimension
                |> MGrid.fill model.walls Wall
                |> MGrid.setAll (List.map (\e -> ( e.location, Enemy_ (toETMStatus e.uid) )) model.enemies)
                |> MGrid.set model.player (Player isPlayerSelected model.playerHp)
    in
    div [ class "center code f2 bg-black white pa3 br3" ]
        (grid
            |> MGrid.viewRows (\_ -> div [ class "flex" ])
                (\_ slot -> viewSlot slot)
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
