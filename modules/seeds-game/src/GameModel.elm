module GameModel exposing
    ( Cell(..)
    , GameModel
    , Info
    , MoveResult(..)
    , info
    , init
    , makeMove
    , selectionPop
    , selectionPush
    )

import Basics.Extra exposing (atLeast, swap)
import Dict
import Grid exposing (GI, Grid)
import List.Extra
import Random
import Random.Extra



-- CELL GRID


type Cell
    = Water
    | Seed
    | Wall
    | Empty


initGrid : Grid Cell
initGrid =
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


computeFallenGrid : Grid Cell -> Grid Cell
computeFallenGrid grid0 =
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
        |> Tuple.second


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



-- SELECTION UTILS


computeValidSelectionIndices : Grid Cell -> List GI -> List GI
computeValidSelectionIndices grid selectionStack =
    case selectionStack of
        [] ->
            Grid.toListBy
                (\i c ->
                    if canStartSelectionWithCell c then
                        Just i

                    else
                        Nothing
                )
                grid
                |> List.filterMap identity

        last :: [] ->
            adjacentOf last
                |> List.filter
                    (\adj -> areCellsAtIndicesConnectible last adj grid)

        last :: secondLast :: _ ->
            adjacentOf last
                |> List.filter
                    (\adj ->
                        (adj /= secondLast)
                            && areCellsAtIndicesConnectible last adj grid
                    )


canStartSelectionWithCell =
    isCellMovable


adjacentOf : ( number, number ) -> List ( number, number )
adjacentOf ( x, y ) =
    [ ( x, y - 1 ), ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ) ]


areCellsAtIndicesConnectible : GI -> GI -> Grid Cell -> Bool
areCellsAtIndicesConnectible a b grid =
    isAdj a b
        && (Maybe.map2 areCellsConnectible (Grid.get a grid) (Grid.get b grid)
                |> Maybe.withDefault False
           )


isAdj ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
    (dx == 0 && dy == 1) || (dy == 0 && dx == 1)


areCellsConnectible : Cell -> Cell -> Bool
areCellsConnectible cell1 cell2 =
    case ( cell1, cell2 ) of
        ( Water, Water ) ->
            True

        ( Seed, Seed ) ->
            True

        _ ->
            False



-- GAME MODEL


type GameModel
    = GM GameState


type alias GameState =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    , grid : Grid Cell
    , selectionStack : List GI
    , random : Random.Seed
    }


init : GameModel
init =
    GM
        { movesLeft = 10
        , targetSeeds = 35
        , targetWater = 35
        , grid = initGrid
        , selectionStack = []
        , random = Random.initialSeed 0
        }


type alias SelectionStack =
    List GI


selectionPush_ : GI -> Grid Cell -> SelectionStack -> Maybe SelectionStack
selectionPush_ idx grid selectionStack =
    if List.member idx (computeValidSelectionIndices grid selectionStack) then
        Just (idx :: selectionStack)

    else
        Nothing


selectionPop_ : SelectionStack -> Maybe SelectionStack
selectionPop_ selectionStack =
    case selectionStack of
        [] ->
            Nothing

        _ :: prevStack ->
            Just prevStack


selectionPush : GI -> GameModel -> Maybe GameModel
selectionPush idx (GM gm) =
    if List.member idx (computeValidSelectionIndices gm.grid gm.selectionStack) then
        Just (GM { gm | selectionStack = idx :: gm.selectionStack })

    else
        Nothing


selectionPop : GameModel -> Maybe GameModel
selectionPop (GM gm) =
    case gm.selectionStack of
        [] ->
            Nothing

        _ :: prevStack ->
            GM { gm | selectionStack = prevStack } |> Just


type alias Info =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    , grid : Grid Cell
    , selectionStack : List GI
    , validIndices : List GI
    }


info : GameModel -> Info
info (GM gm) =
    { movesLeft = gm.movesLeft
    , targetSeeds = gm.targetSeeds
    , targetWater = gm.targetWater
    , grid = gm.grid
    , selectionStack = gm.selectionStack
    , validIndices = computeValidSelectionIndices gm.grid gm.selectionStack
    }


type MoveResult
    = InvalidMove
    | GameLost Info
    | GameWon Info
    | NextState GameModel


makeMove : GameModel -> MoveResult
makeMove (GM gm) =
    if List.length gm.selectionStack < 2 then
        InvalidMove

    else
        let
            ( ct, collectedGrid_ ) =
                collectIndices gm.selectionStack gm.grid

            fallenGrid_ =
                computeFallenGrid collectedGrid_

            ( filledGrid, nextRandom ) =
                Random.step (fillEmptyCells fallenGrid_) gm.random

            nextTargetSeeds =
                (gm.targetSeeds - ct.seeds)
                    |> atLeast 0

            nextTargetWater =
                (gm.targetWater - ct.water)
                    |> atLeast 0

            nextMovesLeft =
                (gm.movesLeft - 1)
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
                , selectionStack = []
                , validIndices = []
                }

        else if isGameLost then
            GameLost
                { targetSeeds = nextTargetSeeds
                , targetWater = nextTargetWater
                , movesLeft = nextMovesLeft
                , grid = filledGrid
                , selectionStack = []
                , validIndices = []
                }

        else
            NextState
                (GM
                    { targetSeeds = nextTargetSeeds
                    , targetWater = nextTargetWater
                    , movesLeft = nextMovesLeft
                    , grid = filledGrid
                    , random = nextRandom
                    , selectionStack = []
                    }
                )
