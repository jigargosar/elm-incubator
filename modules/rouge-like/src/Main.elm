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
    fastAnimSpeed


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


slowTimer : Clock -> Timer
slowTimer =
    flip timerInit slowAnimSpeed


defaultTimer : Clock -> Timer
defaultTimer =
    flip timerInit defaultAnimSpeed


fastTimer : Clock -> Timer
fastTimer =
    flip timerInit fastAnimSpeed


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


enemyIdEq : Uid -> Enemy -> Bool
enemyIdEq =
    idEq



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


enemiesSelectSplitById : Uid -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
enemiesSelectSplitById id =
    enemiesSelectSplitBy (enemyIdEq id)


enemiesSelectSplitBy : (Enemy -> Bool) -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
enemiesSelectSplitBy pred enemies =
    List.selectSplit enemies
        |> List.find (\( _, enemy, _ ) -> pred enemy)


enemiesSelectSplitByLocation : Location -> List Enemy -> Maybe ( List Enemy, Enemy, List Enemy )
enemiesSelectSplitByLocation location =
    enemiesSelectSplitBy (enemyLocationEq location)


enemiesFindAt : Location -> List Enemy -> Maybe Enemy
enemiesFindAt location =
    List.find (enemyLocationEq location)



-- Player


type alias Player =
    { location : Location
    , hp : Int
    }


playerDecHp : Player -> Player
playerDecHp player =
    { player | hp = player.hp - 1 |> atLeast 0 }


playerSetHp : Int -> Player -> Player
playerSetHp hp player =
    { player | hp = hp |> atLeast 0 }


playerInit : Location -> Player
playerInit location =
    { location = location
    , hp = initialPlayerHp
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
        { state : State
        , clock : Clock
        , seed : Seed
        }


type AnimState
    = AnimState Timer State


type State
    = WaitingForInput Player (Cons Enemy)
    | PlayerMoving Timer Location Player (List Enemy)
    | PlayerAttackingEnemy Timer Player (SelectSplit Enemy)
    | EnemiesActing Timer PlayerReaction (Cons EnemyAction)
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
    in
    ( { dimension = dimension
      , walls = acc.walls
      , state =
            acc.enemies
                |> List.uncons
                |> Maybe.map (initWaitingForInput acc.player)
                |> Maybe.withDefault (Victory acc.player)
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
            ( case updateStateOnKey key model.clock model model.state of
                Just state ->
                    { model | state = state }

                Nothing ->
                    model
            , Cmd.none
            )

        Tick delta ->
            ( case updateStateOnTick model.clock model model.state of
                Just stateGenerator ->
                    let
                        ( state, seed ) =
                            Random.step stateGenerator model.seed
                    in
                    { model | state = state, seed = seed, clock = clockStep delta model.clock }

                Nothing ->
                    { model | clock = clockStep delta model.clock }
            , Cmd.none
            )


updateStateOnKey : String -> Clock -> WorldMap a -> State -> Maybe State
updateStateOnKey key clock worldMap state =
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
                            initPlayerMoving clock location player (Cons.toList enemies)
                                |> Just

                        Blocked ->
                            Nothing

                        HasEnemy ez ->
                            let
                                attackTimer =
                                    defaultTimer clock
                            in
                            PlayerAttackingEnemy attackTimer player ez
                                |> Just

                        HasPlayer ->
                            Nothing

                Nothing ->
                    case key of
                        " " ->
                            initPlayerMoving clock player.location player (Cons.toList enemies)
                                |> Just

                        _ ->
                            Nothing

        _ ->
            Nothing


updateStateOnTick : Clock -> WorldMap a -> State -> Maybe (Generator State)
updateStateOnTick clock worldMap state =
    case state of
        WaitingForInput _ _ ->
            Nothing

        PlayerMoving timer location player enemies ->
            if timerIsDone clock timer then
                let
                    newPlayer =
                        playerMapLocation (always location) player
                in
                enemies
                    |> List.uncons
                    |> Maybe.unwrap
                        (Random.constant (Victory newPlayer))
                        (initEnemiesActing clock worldMap newPlayer)
                    |> Just

            else
                Nothing

        PlayerAttackingEnemy timer player (( _, enemy, _ ) as ess) ->
            if timerIsDone clock timer then
                initPlayerMoving clock enemy.location player (selectSplitConcatSides ess)
                    |> justConstant

            else
                Nothing

        Victory _ ->
            Nothing

        EnemiesActing timer playerRA emCons ->
            if timerIsDone clock timer then
                case playerRA of
                    PlayerWasAttacked nHp player ->
                        let
                            nPlayer =
                                playerSetHp nHp player
                        in
                        initWaitingForInput nPlayer (emCons |> Cons.map updateMovingEnemy)
                            |> justConstant

            else
                Nothing

        Defeat _ _ ->
            Nothing


initWaitingForInput : Player -> Cons Enemy -> State
initWaitingForInput player neEnemies =
    WaitingForInput player neEnemies


initPlayerMoving : Clock -> Location -> Player -> List Enemy -> State
initPlayerMoving clock location player enemies =
    let
        movingTimer =
            timerInit clock playerMoveAnimSpeed
    in
    PlayerMoving movingTimer location player enemies


initEnemiesActing : Clock -> WorldMap a -> Player -> Cons Enemy -> Generator State
initEnemiesActing clock worldMap player enemyCons =
    let
        toActionTimer nPlayerHp =
            if nPlayerHp == player.hp then
                defaultTimer clock

            else
                slowTimer clock

        getNextEnemyLocations location =
            worldMapAdjacentWalkable location worldMap
    in
    enemyActionsGeneratorHelp getNextEnemyLocations player enemyCons
        |> generatorWithIndependentSeed
        |> Random.map
            (\( nPlayerHp, eas ) ->
                EnemiesActing
                    (toActionTimer nPlayerHp)
                    (PlayerWasAttacked nPlayerHp player)
                    eas
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


filterGenerator : (a -> Bool) -> List a -> Maybe (Generator a)
filterGenerator f xs =
    Debug.todo "impl"


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
        , case model.state of
            WaitingForInput _ _ ->
                Sub.none

            _ ->
                Browser.Events.onAnimationFrameDelta Tick
        ]



-- View


view : Model -> Html Msg
view model =
    div [ class "measure center" ]
        [ div [ class "pv3 f3" ] [ text "Elm Rouge" ]
        , div [ class "flex relative" ]
            [ viewGrid model
            , viewOverlay model.state
            ]
        ]


viewOverlay : State -> HM
viewOverlay state =
    case state of
        Victory _ ->
            div
                [ class "absolute w-100 h-100 flex items-center justify-center"
                ]
                [ div [ class "bg-white-50 black pa3 br3" ]
                    [ div [ class "code f2 tc" ] [ text "You Won!" ]
                    , div [ class "code f3 tc" ] [ text "Ctrl+R to restart" ]
                    ]
                ]

        _ ->
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
             , case model.state of
                WaitingForInput player enemiesNE ->
                    List.map (\enemy -> viewEnemyTile enemy.location) (Cons.toList enemiesNE)
                        ++ [ viewPlayerTile player.location player.hp ]

                PlayerMoving timer to player enemies ->
                    let
                        progress =
                            timerProgress model.clock timer
                    in
                    List.map (\enemy -> viewEnemyTile enemy.location) enemies
                        ++ [ viewPlayerTileMoving progress to player.location player.hp ]

                PlayerAttackingEnemy timer player ess ->
                    let
                        progress =
                            timerProgress model.clock timer
                    in
                    (selectSplitMapCS
                        (\enemy -> viewEnemyDyingTile progress enemy.location)
                        (\enemy -> viewEnemyTile enemy.location)
                        ess
                        |> selectSplitToList
                    )
                        ++ [ viewPlayerTile player.location player.hp ]

                EnemiesActing timer playerRA eaCons ->
                    let
                        progress =
                            timerProgress model.clock timer
                    in
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
