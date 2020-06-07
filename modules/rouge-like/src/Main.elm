module Main exposing (main)

import Basics.More exposing (..)
import Browser
import Browser.Events
import Cons exposing (Cons)
import Dimension exposing (Dimension)
import Ease
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as JD
import List.Extra as List
import Location exposing (Location)
import Maybe.Extra as Maybe
import Random exposing (Generator, Seed)
import Random.Extra as Random
import Random.List
import Tuple exposing (..)
import Tuple.More as Tuple



-- Config


initialPlayerHp =
    3


initialEnemyCount =
    8


playerMoveAnimSpeed =
    Fast


defaultAnimSpeed =
    fastAnimSpeed



-- Anim Speed Config


fastAnimSpeed =
    ticksToMillis 5


slowAnimSpeed =
    ticksToMillis 25


ticksToMillis =
    mul (1000 / 60)



-- Timer


type alias Clock =
    { time : Float
    , prevTime : Float
    }


clockZero : Clock
clockZero =
    { time = 0, prevTime = 0 }


clockStep : Float -> Clock -> Clock
clockStep delta clock =
    if delta > 0 then
        { clock | time = clock.time + delta, prevTime = clock.time }

    else
        clock


clockCurrentTime : Clock -> Float
clockCurrentTime clock =
    clock.time


clockPreviousTime : Clock -> Float
clockPreviousTime clock =
    clock.prevTime


type alias Timer =
    { startTime : Float
    , duration : Float
    }


timerInit : Clock -> Float -> Timer
timerInit clock duration =
    { startTime = clockCurrentTime clock, duration = duration }


timerIsDone : Clock -> Timer -> Bool
timerIsDone clock timer =
    timerElapsed clock timer >= timer.duration


timerWasDone : Clock -> Timer -> Bool
timerWasDone clock timer =
    timerPrevClockElapsed clock timer >= timer.duration


timerElapsed : Clock -> Timer -> Float
timerElapsed clock timer =
    clockCurrentTime clock - timer.startTime


timerPrevClockElapsed : Clock -> Timer -> Float
timerPrevClockElapsed clock timer =
    clockPreviousTime clock - timer.startTime


timerProgress : Clock -> Timer -> Float
timerProgress clock timer =
    let
        elapsed =
            clockCurrentTime clock - timer.startTime
    in
    clamp 0 timer.duration elapsed / timer.duration



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


initEnemy : Location -> Uid -> Enemy
initEnemy location id =
    { id = id
    , location = location
    }


newEnemy : Location -> Generator Enemy
newEnemy location =
    newUid |> Random.map (initEnemy location)


enemyLocationEq : Location -> Enemy -> Bool
enemyLocationEq location =
    propEq location .location


enemySetLocation : Location -> Enemy -> Enemy
enemySetLocation location enemy =
    { enemy | location = location }



--enemyIdEq : Uid -> Enemy -> Bool
--enemyIdEq =
--    idEq
-- Enemies
--enemiesRemove : Uid -> List Enemy -> List Enemy
--enemiesRemove uid =
--    List.filterNot (enemyIdEq uid)
--
--
--enemiesUpdate : Uid -> (Enemy -> Enemy) -> List Enemy -> List Enemy
--enemiesUpdate id =
--    List.updateIf (enemyIdEq id)
--
--
--enemiesFind : Uid -> List Enemy -> Maybe Enemy
--enemiesFind uid enemies =
--    List.find (enemyIdEq uid) enemies
--
--
--enemiesToIds : List Enemy -> List Uid
--enemiesToIds =
--    List.map .id
--
--
--enemiesSelectSplitById : Uid -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
--enemiesSelectSplitById id =
--    enemiesSelectSplitBy (enemyIdEq id)


enemiesSelectSplitBy : (Enemy -> Bool) -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
enemiesSelectSplitBy pred enemies =
    List.selectSplit enemies
        |> List.find (\( _, enemy, _ ) -> pred enemy)


enemiesSelectSplitByLocation : Location -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
enemiesSelectSplitByLocation location =
    enemiesSelectSplitBy (enemyLocationEq location)



--enemiesFindAt : Location -> List Enemy -> Maybe Enemy
--enemiesFindAt location =
--    List.find (enemyLocationEq location)
-- Player


type alias Player =
    { location : Location
    , hp : Int
    }



--playerDecHp : Player -> Player
--playerDecHp player =
--    { player | hp = player.hp - 1 |> atLeast 0 }


playerSetHp : Int -> Player -> Player
playerSetHp hp player =
    { player | hp = hp |> atLeast 0 }


playerInit : Location -> Player
playerInit location =
    { location = location
    , hp = initialPlayerHp
    }



--playerAlive : Player -> Bool
--playerAlive player =
--    player.hp > 0


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
    shuffleSplit initialEnemyCount acc.empty
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



-- World Map


type alias WorldMap a =
    { a
        | dimension : Dimension
        , walls : List Location
    }


worldMapIsLocationBlocked : Location -> WorldMap a -> Bool
worldMapIsLocationBlocked location worldMap =
    not (Dimension.containsLocation location worldMap.dimension)
        || List.member location worldMap.walls


worldMapIsLocationWalkable : Location -> WorldMap a -> Bool
worldMapIsLocationWalkable location =
    worldMapIsLocationBlocked location >> not


worldMapAdjacentWalkable : Location -> WorldMap a -> List Location
worldMapAdjacentWalkable location worldMap =
    Location.adjacent location
        |> List.filter (\x -> worldMapIsLocationWalkable x worldMap)



-- Model


type alias Model =
    WorldMap
        { animState : AnimState
        , clock : Clock
        , seed : Seed
        }


type AnimState
    = AnimState Timer State


type AnimSpeed
    = Instant
    | Slow
    | Default
    | Fast


type NextState
    = NextState AnimSpeed State


toAnimState : Clock -> NextState -> AnimState
toAnimState clock (NextState speed state) =
    let
        fromDuration duration =
            AnimState (timerInit clock duration) state
    in
    case speed of
        Instant ->
            fromDuration 1

        Slow ->
            fromDuration slowAnimSpeed

        Default ->
            fromDuration defaultAnimSpeed

        Fast ->
            fromDuration fastAnimSpeed


type State
    = WaitingForInput Player (Cons Enemy)
    | PlayerMoving Location Player (List Enemy)
    | PlayerAttackingEnemy Player (SelectSplit Enemy)
    | EnemiesActing PlayerReaction (Cons EnemyAction)
    | Victory Player
    | Defeat Location (List Enemy)


type EnemyAction
    = EnemyMoving Location Enemy


type PlayerReaction
    = PlayerWasAttacked Int Player


type alias SelectSplit a =
    ( List a, a, List a )


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dimension =
            Dimension.new 12 16

        initialSeed =
            Random.initialSeed (flags.now |> always 4)

        ( acc, seed ) =
            Random.step (initialWorldGenerator dimension) initialSeed

        initialClock =
            clockZero
    in
    ( { dimension = dimension
      , walls = acc.walls
      , animState =
            acc.enemies
                |> List.uncons
                |> Maybe.map (initWaitingForInput acc.player)
                |> Maybe.withDefault
                    (Victory acc.player
                        |> NextState Instant
                    )
                |> toAnimState initialClock
      , clock = initialClock
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
            ( case model.animState of
                AnimState timer state ->
                    if timerIsDone model.clock timer then
                        case updateStateOnKey key model state of
                            Just nextState ->
                                { model | animState = toAnimState model.clock nextState }

                            Nothing ->
                                model

                    else
                        model
            , Cmd.none
            )

        Tick delta ->
            ( case model.animState of
                AnimState timer state ->
                    if timerIsDone model.clock timer then
                        case updateStateOnTimerDone model state of
                            Just stateGenerator ->
                                let
                                    ( nextState, seed ) =
                                        Random.step stateGenerator model.seed
                                in
                                { model
                                    | animState = toAnimState model.clock nextState
                                    , seed = seed
                                    , clock = clockStep delta model.clock
                                }

                            Nothing ->
                                { model | clock = clockStep delta model.clock }

                    else
                        { model | clock = clockStep delta model.clock }
            , Cmd.none
            )


updateStateOnKey : String -> WorldMap a -> State -> Maybe NextState
updateStateOnKey key worldMap state =
    case state of
        WaitingForInput player enemies ->
            case directionFromKey key of
                Just direction ->
                    let
                        location =
                            stepLocationInDirection direction player.location
                    in
                    case classifyLocation location player enemies worldMap of
                        HasNoActor ->
                            initPlayerMoving location player (Cons.toList enemies)
                                |> Just

                        Blocked ->
                            Nothing

                        HasEnemy ez ->
                            PlayerAttackingEnemy player ez
                                |> NextState Default
                                |> Just

                        HasPlayer ->
                            Nothing

                Nothing ->
                    case key of
                        " " ->
                            initPlayerMoving player.location player (Cons.toList enemies)
                                |> Just

                        _ ->
                            Nothing

        _ ->
            Nothing


updateStateOnTimerDone : WorldMap a -> State -> Maybe (Generator NextState)
updateStateOnTimerDone worldMap state =
    case state of
        WaitingForInput _ _ ->
            Nothing

        PlayerMoving location player enemies ->
            let
                newPlayer =
                    playerMapLocation (always location) player
            in
            enemies
                |> List.uncons
                |> Maybe.unwrap
                    (Random.constant
                        (Victory newPlayer
                            |> NextState Instant
                        )
                    )
                    (initEnemiesActing worldMap newPlayer)
                |> Just

        PlayerAttackingEnemy player (( _, enemy, _ ) as ess) ->
            initPlayerMoving enemy.location player (selectSplitConcatSides ess)
                |> justConstant

        Victory _ ->
            Nothing

        EnemiesActing playerRA emCons ->
            case playerRA of
                PlayerWasAttacked nHp player ->
                    let
                        nPlayer =
                            playerSetHp nHp player

                        nEnemyCons =
                            emCons |> Cons.map updateMovingEnemy

                        nextState =
                            if nHp == 0 then
                                Defeat player.location (Cons.toList nEnemyCons)
                                    |> NextState Slow

                            else
                                initWaitingForInput nPlayer nEnemyCons
                    in
                    nextState
                        |> justConstant

        Defeat _ _ ->
            Nothing


initWaitingForInput : Player -> Cons Enemy -> NextState
initWaitingForInput player neEnemies =
    WaitingForInput player neEnemies
        |> NextState Instant


initPlayerMoving : Location -> Player -> List Enemy -> NextState
initPlayerMoving location player enemies =
    PlayerMoving location player enemies
        |> NextState playerMoveAnimSpeed


initEnemiesActing : WorldMap a -> Player -> Cons Enemy -> Generator NextState
initEnemiesActing worldMap player enemyCons =
    let
        getNextEnemyLocations location =
            worldMapAdjacentWalkable location worldMap
    in
    enemyActionsGeneratorHelp getNextEnemyLocations player enemyCons
        |> generatorWithIndependentSeed
        |> Random.map
            (\( nPlayerHp, eas ) ->
                let
                    speed =
                        if nPlayerHp == player.hp then
                            Default

                        else
                            Slow
                in
                EnemiesActing
                    (PlayerWasAttacked nPlayerHp player)
                    eas
                    |> NextState speed
            )


generatorWithIndependentSeed : (Seed -> ( b, Seed )) -> Generator b
generatorWithIndependentSeed f =
    Random.independentSeed
        |> Random.map (f >> Tuple.first)


enemyActionsGeneratorHelp :
    (Location -> List Location)
    -> Player
    -> Cons Enemy
    -> Seed
    -> ( ( Int, Cons EnemyAction ), Seed )
enemyActionsGeneratorHelp getNextLocations player iEnemies iSeed =
    let
        iOccupied : List Location
        iOccupied =
            iEnemies
                |> Cons.toList
                |> List.map .location

        reducer : ( Int, List Location, Seed ) -> Enemy -> ( ( Int, List Location, Seed ), EnemyAction )
        reducer ( playerHp, occupied, seed ) enemy =
            let
                walkableLocations =
                    enemy.location
                        |> getNextLocations

                nextLocationGenerator =
                    if List.member player.location walkableLocations then
                        Random.constant player.location

                    else
                        walkableLocations
                            |> listRemoveAll occupied
                            |> maybeUniformGenerator
                            |> Maybe.withDefault (Random.constant enemy.location)

                ( targetLocation, nSeed ) =
                    Random.step nextLocationGenerator seed
            in
            if targetLocation == player.location then
                ( ( playerHp - 1 |> atLeast 0, occupied, nSeed )
                , EnemyMoving enemy.location enemy
                )

            else
                ( ( playerHp
                  , List.setIf (eq enemy.location) targetLocation occupied
                  , nSeed
                  )
                , EnemyMoving targetLocation enemy
                )
    in
    Cons.mapAccuml reducer ( player.hp, iOccupied, iSeed ) iEnemies
        |> (\( ( rPlayer, _, rSeed ), rEAs ) -> ( ( rPlayer, rEAs ), rSeed ))


listRemoveAll : List a -> List a -> List a
listRemoveAll toRemove =
    let
        shouldKeep x =
            List.notMember x toRemove
    in
    List.filter shouldKeep


justConstant : a -> Maybe (Generator a)
justConstant x =
    Random.constant x
        |> Just


updateMovingEnemy : EnemyAction -> Enemy
updateMovingEnemy enemyMoving =
    case enemyMoving of
        EnemyMoving location enemy ->
            enemySetLocation location enemy


selectSplitConcatSides ( l, _, r ) =
    l ++ r


selectSplitToList ( l, c, r ) =
    l ++ c :: r


selectSplitMapCS fc fs ( l, c, r ) =
    ( List.map fs l, fc c, List.map fs r )


type LocationClass
    = Blocked
    | HasEnemy ( List Enemy, Enemy, List Enemy )
    | HasPlayer
    | HasNoActor


classifyLocation : Location -> Player -> Cons Enemy -> WorldMap a -> LocationClass
classifyLocation location player enemies worldMap =
    if worldMapIsLocationBlocked location worldMap then
        Blocked

    else if playerLocationEq location player then
        HasPlayer

    else
        enemiesSelectSplitByLocation location (Cons.toList enemies)
            |> Maybe.unwrap HasNoActor HasEnemy


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
        , case model.animState of
            AnimState timer _ ->
                if timerWasDone model.clock timer then
                    Sub.none

                else
                    Browser.Events.onAnimationFrameDelta Tick
        ]



-- View


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Elm Rouge" ]
        , div [ class "flex relative" ]
            [ viewGrid model
            , case model.animState of
                AnimState timer state ->
                    let
                        progress =
                            timerProgress model.clock timer
                    in
                    case state of
                        Victory _ ->
                            viewOverlayMsg progress "You Won!"

                        Defeat _ _ ->
                            viewOverlayMsg progress "You Lost!"

                        _ ->
                            text ""
            ]
        ]


viewOverlayMsg progress message =
    div
        [ class "absolute w-100 h-100 flex items-center justify-center"
        , cssFade progress
        ]
        [ div [ class "bg-white-50 black pa3 br3" ]
            [ div [ class "code f2 tc" ] [ text message ]
            , div [ class "code f3 tc" ] [ text "Ctrl+R to restart" ]
            ]
        ]


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


cssFade o =
    style "opacity" (String.fromFloat o)


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


viewPlayerTileFading progress location hp =
    div
        [ commonStyles
        , cssTransform [ cssTranslate (locationToDXY location) ]
        , cssFade progress
        ]
        [ text (String.fromInt hp) ]


viewPlayerTileMoving progress to location hp =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad progress)
    in
    div
        [ commonStyles
        , cssTransform
            [ cssTranslate (locationToDXY location)
            , cssTranslate moveDXY
            ]
        ]
        [ text (String.fromInt hp) ]


viewEnemyTileMoving progress to location =
    let
        moveDXY =
            ( to, location )
                |> Tuple.map Location.toTuple
                |> uncurry Tuple.sub
                |> Tuple.toFloatScaled (32 * Ease.outQuad progress)
    in
    div
        [ commonStyles
        , cssTransform
            [ cssTranslate (locationToDXY location)
            , cssTranslate moveDXY
            ]
        ]
        [ text "e" ]


viewEnemyTile location =
    div
        [ commonStyles, cssTransform [ cssTranslate (locationToDXY location) ] ]
        [ text "e" ]


viewEnemyDyingTile progress location =
    div
        [ commonStyles
        , cssTransform
            [ cssTranslate (locationToDXY location)
            , cssScale (Ease.reverse Ease.inOutBounce progress)
            ]
        ]
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
             , case model.animState of
                AnimState timer state ->
                    let
                        progress =
                            timerProgress model.clock timer
                    in
                    case state of
                        WaitingForInput player enemiesNE ->
                            List.map (\enemy -> viewEnemyTile enemy.location) (Cons.toList enemiesNE)
                                ++ [ viewPlayerTile player.location player.hp ]

                        PlayerMoving to player enemies ->
                            List.map (\enemy -> viewEnemyTile enemy.location) enemies
                                ++ [ viewPlayerTileMoving progress to player.location player.hp ]

                        PlayerAttackingEnemy player ess ->
                            (selectSplitMapCS
                                (\enemy -> viewEnemyDyingTile progress enemy.location)
                                (\enemy -> viewEnemyTile enemy.location)
                                ess
                                |> selectSplitToList
                            )
                                ++ [ viewPlayerTile player.location player.hp ]

                        EnemiesActing playerRA eaCons ->
                            (eaCons
                                |> Cons.toList
                                |> List.map
                                    (\ea ->
                                        case ea of
                                            EnemyMoving to enemy ->
                                                viewEnemyTileMoving progress to enemy.location
                                    )
                            )
                                ++ (case playerRA of
                                        PlayerWasAttacked hp player ->
                                            [ viewPlayerTileFading progress player.location hp
                                            , viewPlayerTileFading (Ease.reverse Ease.linear progress) player.location player.hp
                                            ]
                                   )

                        Victory player ->
                            [ viewPlayerTile player.location player.hp ]

                        Defeat location enemies ->
                            List.map (\enemy -> viewEnemyTile enemy.location) enemies
                                ++ [ viewPlayerTile location 0 ]
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
