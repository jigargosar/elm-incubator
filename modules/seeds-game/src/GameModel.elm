module GameModel exposing
    ( Cell(..)
    , CellGrid
    , Entries
    , Entry
    , Model(..)
    , MoveDetails
    , OverModel
    , SelectingModel
    , Stats
    , cellGrid
    , init
    , makeMove
    , selectionPop
    , selectionPush
    , selectionStack
    , stats
    )

import Basics.Extra exposing (atLeast, swap)
import Dict exposing (Dict)
import Grid exposing (GI, Grid)
import List.Extra
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
    { collected : { indexSet : Set GI, water : Int, seeds : Int }
    , fallenLookup : Dict GI GI
    , generated : { indexSet : Set GI, grid : CellGrid }
    }


collectAndGenerateWithDetails : List GI -> CellGrid -> Random.Generator MoveDetails
collectAndGenerateWithDetails collectIndices grid =
    let
        ( collectedEntries, collectedGrid ) =
            collectCellsAtIndices collectIndices grid

        ( fallenIndices, fallenGrid ) =
            computeFallenGrid collectedGrid

        initMoveDetails : { grid : CellGrid, indexSet : Set GI } -> MoveDetails
        initMoveDetails generated =
            let
                collectedCells =
                    List.map Tuple.second collectedEntries

                eq =
                    (==)
            in
            { collected =
                { indexSet = List.map Tuple.first collectedEntries |> Set.fromList
                , water = List.Extra.count (eq Water) collectedCells
                , seeds = List.Extra.count (eq Seed) collectedCells
                }
            , fallenLookup = Dict.fromList fallenIndices
            , generated = generated
            }
    in
    fallenGrid
        |> fillEmptyCells
        |> Random.map initMoveDetails


type alias Entry =
    ( GI, Cell )


type alias Entries =
    List Entry


collectCellsAtIndices : List GI -> CellGrid -> ( Entries, CellGrid )
collectCellsAtIndices indicesToCollect grid =
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
    in
    ( collectedEntries
    , setEmptyAtIndices indicesToEmpty grid
    )


setEmptyAtIndices indicesToEmpty grid =
    Grid.map
        (\i c ->
            if List.member i indicesToEmpty then
                Empty

            else
                c
        )
        grid


computeFallenGrid : CellGrid -> ( List ( GI, GI ), CellGrid )
computeFallenGrid grid0 =
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


fillEmptyCells : CellGrid -> Random.Generator { indexSet : Set GI, grid : CellGrid }
fillEmptyCells grid =
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
    List.Extra.mapAccumr (\a b -> func b a |> swap) acc
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
    = Selecting SelectingModel
    | Over OverModel


type alias SelectingModel =
    Internal { selecting : () }


type alias OverModel =
    Internal { over : () }


type Internal a
    = Internal State


type alias State =
    { stats : Stats
    , grid : CellGrid
    , selection : Selection
    , random : Random.Seed
    }


init : Model
init =
    Internal
        { stats =
            { movesLeft = 2
            , targetSeeds = 35
            , targetWater = 35
            }
        , grid = initCellGrid
        , selection = emptySelection
        , random = Random.initialSeed 0
        }
        |> Selecting


selectionPush : GI -> SelectingModel -> Maybe SelectingModel
selectionPush idx (Internal gm) =
    selectionPush_ idx gm.grid gm.selection
        |> Maybe.map (\selection -> Internal { gm | selection = selection })


selectionPop : SelectingModel -> Maybe SelectingModel
selectionPop (Internal gm) =
    selectionPop_ gm.selection
        |> Maybe.map (\selection -> Internal { gm | selection = selection })


type alias Stats =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    }


stats : Model -> Stats
stats =
    toInternal >> internalStats


cellGrid : Model -> CellGrid
cellGrid =
    toInternal >> internalCellGrid


internalCellGrid : Internal a -> CellGrid
internalCellGrid (Internal modelRecord) =
    modelRecord.grid


internalStats : Internal a -> Stats
internalStats (Internal modelRecord) =
    modelRecord.stats


toInternal : Model -> Internal a
toInternal state =
    case state of
        Selecting (Internal mr) ->
            Internal mr

        Over (Internal mr) ->
            Internal mr


selectionStack : SelectingModel -> List GI
selectionStack (Internal modelRecord) =
    selectionToStack modelRecord.selection


selectionToCollectibleIndices : Selection -> Maybe (List GI)
selectionToCollectibleIndices (Selection stack) =
    if List.length stack < 2 then
        Nothing

    else
        Just stack


makeMove : SelectingModel -> Maybe ( MoveDetails, Model )
makeMove (Internal modelRecord) =
    case selectionToCollectibleIndices modelRecord.selection of
        Nothing ->
            Nothing

        Just collectibleIndices ->
            let
                ( moveDetails, nextRandom ) =
                    Random.step
                        (collectAndGenerateWithDetails collectibleIndices modelRecord.grid)
                        modelRecord.random

                stats_ =
                    modelRecord.stats

                nextTargetSeeds =
                    (stats_.targetSeeds - moveDetails.collected.seeds) |> atLeast 0

                nextTargetWater =
                    (stats_.targetWater - moveDetails.collected.water) |> atLeast 0

                nextMovesLeft =
                    (stats_.movesLeft - 1) |> atLeast 0
            in
            ( moveDetails
            , modelFromState
                { stats =
                    { targetSeeds = nextTargetSeeds
                    , targetWater = nextTargetWater
                    , movesLeft = nextMovesLeft
                    }
                , grid = moveDetails.generated.grid
                , random = nextRandom
                , selection = emptySelection
                }
            )
                |> Just


modelFromState : State -> Model
modelFromState modelRecord =
    let
        nextModel =
            Internal modelRecord

        stats_ =
            modelRecord.stats

        isGameOver =
            (stats_.movesLeft == 0)
                && List.any ((/=) 0) [ stats_.targetWater, stats_.targetSeeds ]
    in
    if isGameOver then
        Over nextModel

    else
        Selecting nextModel
