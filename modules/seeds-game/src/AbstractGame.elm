module AbstractGame exposing
    ( Cell(..)
    , GameModel
    , Info
    , MoveBuilder
    , MoveResult(..)
    , info
    , initGame
    , initMoveBuilder
    , isValidStart
    , makeMove
    , pushIdx
    , toGameModel
    , toStack
    )

-- GAME GRID

import Basics.Extra exposing (atLeast, swap)
import Dict
import Grid exposing (GI, Grid)
import List.Extra
import Maybe.Extra
import Random
import Random.Extra


computeFallingIndicesAndUpdateGrid : Grid Cell -> ( List ( GI, GI ), Grid Cell )
computeFallingIndicesAndUpdateGrid grid0 =
    let
        computeFallingAt : GI -> Grid Cell -> Maybe ( ( GI, GI ), Grid Cell )
        computeFallingAt to grid =
            case Grid.get to grid of
                Just Empty ->
                    case findFirstMovableAbove to grid of
                        Just from ->
                            case Grid.swap from to grid of
                                Just sg ->
                                    Just ( ( from, to ), sg )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
    in
    filterMapAccumr computeFallingAt grid0 (Grid.indices grid0)


findFirstMovableAbove : GI -> Grid Cell -> Maybe GI
findFirstMovableAbove startIndex grid =
    case Grid.entryAbove startIndex grid of
        Nothing ->
            Nothing

        Just ( index, cell ) ->
            if isCellMovable cell then
                Just index

            else
                findFirstMovableAbove index grid


filterMapAccumr : (a -> c -> Maybe ( b, c )) -> c -> List a -> ( List b, c )
filterMapAccumr func acc =
    flipMapAccumr
        (\a b ->
            case func a b of
                Nothing ->
                    ( Nothing, b )

                Just ( na, nb ) ->
                    ( Just na, nb )
        )
        acc
        >> Tuple.mapFirst (List.filterMap identity)


flipMapAccumr : (a -> c -> ( b, c )) -> c -> List a -> ( List b, c )
flipMapAccumr func acc =
    List.Extra.mapAccumr (\a b -> func b a |> swap) acc
        >> swap


filterFoldr : (a -> b -> Maybe b) -> b -> List a -> b
filterFoldr func acc =
    List.foldr
        (\a b ->
            func a b |> Maybe.withDefault b
        )
        acc



-- GAME MODEL


type GameModel
    = GM
        { movesLeft : Int
        , targetSeeds : Int
        , targetWater : Int
        , grid : Grid Cell
        , random : Random.Seed
        }


type Cell
    = Water
    | Seed
    | Wall
    | Empty


initialGrid : Grid Cell
initialGrid =
    let
        wallIndices =
            [ ( 1, 1 ), ( 4, 1 ), ( 1, 4 ), ( 4, 4 ) ]

        isSeedIdx ( x, y ) =
            x == 0 || y == 0 || x == 5 || y == 5

        grid =
            Grid.init
                6
                6
                (\i ->
                    if List.member i wallIndices then
                        Wall

                    else if isSeedIdx i then
                        Seed

                    else
                        Water
                )
    in
    grid


initGame : GameModel
initGame =
    GM
        { movesLeft = 10
        , targetSeeds = 35
        , targetWater = 35
        , grid = initialGrid
        , random = Random.initialSeed 0
        }


type alias Info =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    , grid : Grid Cell
    }


info : GameModel -> Info
info (GM g) =
    Info g.movesLeft g.targetSeeds g.targetWater g.grid


type MoveResult
    = InvalidMove
    | GameLost Info
    | GameWon Info
    | NextState GameModel


isCellMovable : Cell -> Bool
isCellMovable cell =
    case cell of
        Water ->
            True

        Seed ->
            True

        Wall ->
            False

        Empty ->
            False


collectIndices : List GI -> Grid Cell -> ( { seeds : Int, water : Int }, Grid Cell )
collectIndices indicesToCollect grid0 =
    let
        computeCollectedAt idx ( ct, grid ) =
            case Grid.get idx grid of
                Just cell ->
                    let
                        mbNewCt =
                            case cell of
                                Water ->
                                    Just { ct | water = ct.water + 1 }

                                Seed ->
                                    Just { ct | seeds = ct.seeds + 1 }

                                Wall ->
                                    Nothing

                                Empty ->
                                    Nothing

                        mbNewGrid =
                            Grid.set idx Empty grid
                    in
                    Maybe.map2 Tuple.pair mbNewCt mbNewGrid

                Nothing ->
                    Nothing
    in
    filterFoldr computeCollectedAt ( { seeds = 0, water = 0 }, grid0 ) indicesToCollect


fillEmptyCells : Grid Cell -> Random.Generator (Grid Cell)
fillEmptyCells grid =
    Grid.toListBy
        (\i c ->
            if c == Empty then
                Just i

            else
                Nothing
        )
        grid
        |> List.filterMap identity
        |> Random.Extra.traverse
            (\i ->
                Random.uniform Water [ Seed ]
                    |> Random.map (Tuple.pair i)
            )
        |> Random.map Dict.fromList
        |> Random.map
            (\nd ->
                Grid.map (\i c -> Dict.get i nd |> Maybe.withDefault c) grid
            )


isValidStart : GI -> GameModel -> Bool
isValidStart idx (GM gm) =
    isCellMovableAt idx gm.grid


isCellMovableAt : GI -> Grid Cell -> Bool
isCellMovableAt idx grid =
    Grid.get idx grid
        |> Maybe.map isCellMovable
        |> Maybe.withDefault False


type MoveBuilder
    = MoveBuilder GameModel (List GI)


initMoveBuilder : GameModel -> MoveBuilder
initMoveBuilder gm =
    MoveBuilder gm []


pushIdx : GI -> MoveBuilder -> Maybe MoveBuilder
pushIdx idx (MoveBuilder ((GM { grid }) as gm) stack) =
    case stack of
        [] ->
            if isValidStart idx gm then
                MoveBuilder gm [ idx ]
                    |> Just

            else
                Nothing

        last :: _ ->
            if areCellsAtIndicesConnectible idx last grid then
                MoveBuilder gm (idx :: stack)
                    |> Just

            else
                Nothing


toStack : MoveBuilder -> List GI
toStack (MoveBuilder _ stack) =
    stack


toGameModel : MoveBuilder -> GameModel
toGameModel (MoveBuilder gm _) =
    gm


areCellsAtIndicesConnectible : GI -> GI -> Grid Cell -> Bool
areCellsAtIndicesConnectible a b grid =
    isAdj a b
        && (Maybe.map2 areCellsConnectible (Grid.get a grid) (Grid.get b grid)
                |> Maybe.withDefault False
           )


areCellsConnectible : Cell -> Cell -> Bool
areCellsConnectible cell1 cell2 =
    case ( cell1, cell2 ) of
        ( Water, Water ) ->
            True

        ( Seed, Seed ) ->
            True

        _ ->
            False


isAdj ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
    (dx == 0 && dy == 1) || (dy == 0 && dx == 1)


makeMove : List GI -> GameModel -> MoveResult
makeMove input (GM gm) =
    if input == [] then
        InvalidMove

    else
        let
            ( ct, collectedGrid_ ) =
                collectIndices input gm.grid

            ( _, fallenGrid_ ) =
                computeFallingIndicesAndUpdateGrid collectedGrid_

            ( filledGrid, nextRandom ) =
                Random.step (fillEmptyCells fallenGrid_) gm.random

            nextTargetSeeds =
                gm.targetSeeds
                    - ct.seeds
                    |> atLeast 0

            nextTargetWater =
                gm.targetWater
                    - ct.water
                    |> atLeast 0

            nextMovesLeft =
                gm.movesLeft
                    - 1
                    |> atLeast 0

            isGameWon =
                List.all ((==) 0) [ nextTargetSeeds, nextTargetWater ]

            isGameLost =
                not isGameWon && nextMovesLeft == 0
        in
        if isGameWon then
            GameWon
                { targetSeeds = nextTargetSeeds
                , targetWater = nextTargetWater
                , movesLeft = nextMovesLeft
                , grid = filledGrid
                }

        else if isGameLost then
            GameLost
                { targetSeeds = nextTargetSeeds
                , targetWater = nextTargetWater
                , movesLeft = nextMovesLeft
                , grid = filledGrid
                }

        else
            NextState
                (GM
                    { targetSeeds = nextTargetSeeds
                    , targetWater = nextTargetWater
                    , movesLeft = nextMovesLeft
                    , grid = filledGrid
                    , random = nextRandom
                    }
                )
