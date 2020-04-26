module GameModel exposing
    ( Cell(..)
    , CellGrid
    , Model
    , MoveDetails
    , Stats
    , cellGrid
    , init
    , isOver
    , makeMove
    , selectionPop
    , selectionPush
    , selectionStack
    , stats
    )

import Basics.Extra exposing (..)
import Dict exposing (Dict)
import Grid exposing (GI, Grid)
import List.Extra as List
import Random
import Set exposing (Set)



-- CELL GRID


type alias CellGrid =
    Grid.Grid Cell


type Cell
    = Water
    | Seed
    | Wall
    | Empty


initCellGrid : CellGrid
initCellGrid =
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


type alias MoveDetails =
    { initial : CellGrid
    , collected : Collected
    , fallen : Fallen
    , generated : Generated
    }


type alias Collected =
    { indexSet : Set GI, water : Int, seeds : Int, grid : CellGrid }


type alias Fallen =
    { lookup : Dict GI GI, grid : CellGrid }


type alias Generated =
    { indexSet : Set GI, grid : CellGrid }


moveDetailsGenerator : List GI -> CellGrid -> Random.Generator MoveDetails
moveDetailsGenerator indicesToCollect grid =
    let
        collected =
            computeCollected indicesToCollect grid

        fallen =
            computeFallen collected.grid

        initMoveDetails : Generated -> MoveDetails
        initMoveDetails generated =
            { initial = grid
            , collected = collected
            , fallen = fallen
            , generated = generated
            }
    in
    computeGenerated fallen.grid
        |> Random.map initMoveDetails


type alias Entry =
    ( GI, Cell )


type alias Entries =
    List Entry


computeCollected : List GI -> CellGrid -> Collected
computeCollected indicesToCollect grid =
    let
        collectedEntries : List ( GI, Cell )
        collectedEntries =
            grid
                |> Grid.toList
                |> List.filter
                    (\( i, cell ) ->
                        List.member i indicesToCollect && List.member cell [ Water, Seed ]
                    )

        indicesToEmpty =
            List.map Tuple.first collectedEntries

        collectedCells =
            List.map Tuple.second collectedEntries
    in
    { indexSet = List.map Tuple.first collectedEntries |> Set.fromList
    , water = List.count (eq Water) collectedCells
    , seeds = List.count (eq Seed) collectedCells
    , grid = setEmptyAtIndices indicesToEmpty grid
    }


eq =
    (==)


setEmptyAtIndices indicesToEmpty grid =
    Grid.map
        (\i c ->
            if List.member i indicesToEmpty then
                Empty

            else
                c
        )
        grid


computeFallen : CellGrid -> Fallen
computeFallen grid0 =
    let
        computeFallingAt : GI -> CellGrid -> Maybe ( ( GI, GI ), CellGrid )
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
        |> (\( fallenIndexPairs, grid ) ->
                { lookup = Dict.fromList fallenIndexPairs, grid = grid }
           )


computeGenerated : CellGrid -> Random.Generator Generated
computeGenerated grid =
    let
        emptyIndices =
            grid
                |> Grid.toList
                |> List.filterMap
                    (\( i, c ) ->
                        if c == Empty then
                            Just i

                        else
                            Nothing
                    )

        cellGenerator =
            Random.uniform Water [ Seed ]
    in
    Random.list (List.length emptyIndices) cellGenerator
        |> Random.map
            (\cells ->
                let
                    generatedCellsLookup =
                        List.map2 Tuple.pair emptyIndices cells
                            |> Dict.fromList
                in
                { indexSet = Set.fromList emptyIndices
                , grid = Grid.map (\i c -> Dict.get i generatedCellsLookup |> Maybe.withDefault c) grid
                }
            )


findFirstMovableAbove : GI -> CellGrid -> Maybe GI
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
    List.mapAccumr (\a b -> func b a |> swap) acc
        >> swap



-- SELECTION


type Selection
    = Selection (List GI)


emptySelection =
    Selection []


selectionToStack (Selection stack) =
    stack


selectionPush_ : GI -> CellGrid -> Selection -> Maybe Selection
selectionPush_ idx grid ((Selection stack) as selection) =
    if List.member idx (computeValidSelectionIndices grid selection) then
        Just (Selection (idx :: stack))

    else
        Nothing


selectionPop_ : Selection -> Maybe Selection
selectionPop_ (Selection stack) =
    case stack of
        [] ->
            Nothing

        _ :: prevStack ->
            Just (Selection prevStack)


computeValidSelectionIndices : CellGrid -> Selection -> List GI
computeValidSelectionIndices grid (Selection stack) =
    case stack of
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


areCellsAtIndicesConnectible : GI -> GI -> CellGrid -> Bool
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


type Model
    = Internal State


type alias State =
    { stats : Stats
    , grid : CellGrid
    , selection_ : Selection
    , random : Random.Seed
    }


init : Model
init =
    fromState
        { stats =
            { movesLeft = 2
            , targetSeeds = 35
            , targetWater = 35
            }
        , grid = initCellGrid
        , selection_ = emptySelection
        , random = Random.initialSeed 0
        }


isGameOver_ : State -> Bool
isGameOver_ state =
    let
        { movesLeft, targetWater, targetSeeds } =
            state.stats
    in
    (movesLeft == 0)
        && List.any ((/=) 0) [ targetWater, targetSeeds ]


isOver : Model -> Bool
isOver =
    unwrap >> isGameOver_


fromState : State -> Model
fromState =
    Internal


selection_ state =
    if isGameOver_ state then
        Nothing

    else
        Just state.selection_


selectionPush : GI -> Model -> Maybe Model
selectionPush idx (Internal state) =
    selection_ state
        |> Maybe.andThen (selectionPush_ idx state.grid)
        |> Maybe.map (\selection -> Internal { state | selection_ = selection })


selectionPop : Model -> Maybe Model
selectionPop (Internal state) =
    selection_ state
        |> Maybe.andThen selectionPop_
        |> Maybe.map (\selection -> Internal { state | selection_ = selection })


type alias Stats =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    }


stats : Model -> Stats
stats =
    unwrap >> .stats


cellGrid : Model -> CellGrid
cellGrid =
    unwrap >> .grid


unwrap : Model -> State
unwrap (Internal state) =
    state


selectionStack : Model -> List GI
selectionStack (Internal state) =
    selection_ state
        |> Maybe.map selectionToStack
        |> Maybe.withDefault []


makeMove : Model -> Maybe ( MoveDetails, Model )
makeMove (Internal state) =
    selection_ state
        |> Maybe.andThen selectionToCollectibleIndices
        |> Maybe.map (flip makeMoveHelp state)


selectionToCollectibleIndices : Selection -> Maybe (List GI)
selectionToCollectibleIndices (Selection stack) =
    if List.length stack < 2 then
        Nothing

    else
        Just stack


makeMoveHelp : List GI -> State -> ( MoveDetails, Model )
makeMoveHelp collectibleIndices state =
    let
        ( moveDetails, nextRandom ) =
            Random.step
                (moveDetailsGenerator collectibleIndices state.grid)
                state.random

        nextTargetSeeds =
            (state.stats.targetSeeds - moveDetails.collected.seeds) |> atLeast 0

        nextTargetWater =
            (state.stats.targetWater - moveDetails.collected.water) |> atLeast 0

        nextMovesLeft =
            (state.stats.movesLeft - 1) |> atLeast 0
    in
    ( moveDetails
    , fromState
        { stats =
            { targetSeeds = nextTargetSeeds
            , targetWater = nextTargetWater
            , movesLeft = nextMovesLeft
            }
        , grid = moveDetails.generated.grid
        , random = nextRandom
        , selection_ = emptySelection
        }
    )


computeNextStats : Collected -> Stats -> Stats
computeNextStats collected { movesLeft, targetSeeds, targetWater } =
    { targetSeeds = targetSeeds - collected.seeds |> atLeast 0
    , targetWater = (targetWater - collected.water) |> atLeast 0
    , movesLeft = movesLeft - 1 |> atLeast 0
    }
