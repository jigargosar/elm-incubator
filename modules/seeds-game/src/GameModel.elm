module GameModel exposing
    ( Cell(..)
    , Info
    , Model
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


initCellGrid : Grid Cell
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


collectAndGenerateNextGrid :
    List GI
    -> Grid Cell
    -> Random.Generator ( { seeds : Int, water : Int }, Grid Cell )
collectAndGenerateNextGrid collectIndices grid =
    let
        ( ct, collectedGrid ) =
            collectCellsAtIndices collectIndices grid
    in
    computeFallenGrid collectedGrid
        |> fillEmptyCells
        |> Random.map (Tuple.pair ct)


collectCellsAtIndices : List GI -> Grid Cell -> ( { seeds : Int, water : Int }, Grid Cell )
collectCellsAtIndices indicesToCollect grid0 =
    let
        collectAt idx ( ct, grid ) =
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

        collectAtIgnoreNothing idx acc =
            collectAt idx acc |> Maybe.withDefault acc
    in
    List.foldr collectAtIgnoreNothing ( { seeds = 0, water = 0 }, grid0 ) indicesToCollect


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



-- SELECTION


type Selection
    = Selection (List GI)


emptySelection =
    Selection []


selectionToStack (Selection stack) =
    stack


selectionPush_ : GI -> Grid Cell -> Selection -> Maybe Selection
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



-- SELECTION HELPERS


computeValidSelectionIndices : Grid Cell -> Selection -> List GI
computeValidSelectionIndices grid (Selection selectionStack) =
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


type Model
    = Model State


type alias State =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    , grid : Grid Cell
    , selection : Selection
    , random : Random.Seed
    }


init : Model
init =
    Model
        { movesLeft = 10
        , targetSeeds = 35
        , targetWater = 35
        , grid = initCellGrid
        , selection = emptySelection
        , random = Random.initialSeed 0
        }


selectionPush : GI -> Model -> Maybe Model
selectionPush idx (Model gm) =
    selectionPush_ idx gm.grid gm.selection
        |> Maybe.map (\selection -> Model { gm | selection = selection })


selectionPop : Model -> Maybe Model
selectionPop (Model gm) =
    selectionPop_ gm.selection
        |> Maybe.map (\selection -> Model { gm | selection = selection })


type alias Info =
    { movesLeft : Int
    , targetSeeds : Int
    , targetWater : Int
    , grid : Grid Cell
    , selectionStack : List GI
    , validIndices : List GI
    }


info : Model -> Info
info (Model gm) =
    { movesLeft = gm.movesLeft
    , targetSeeds = gm.targetSeeds
    , targetWater = gm.targetWater
    , grid = gm.grid
    , selectionStack = selectionToStack gm.selection
    , validIndices = computeValidSelectionIndices gm.grid gm.selection
    }


type MoveResult
    = InvalidMove
    | GameLost Info
    | GameWon Info
    | NextState Model


selectionToCollectibleIndices : Selection -> Maybe (List GI)
selectionToCollectibleIndices (Selection stack) =
    if List.length stack < 2 then
        Nothing

    else
        Just stack


makeMove : Model -> MoveResult
makeMove (Model gm) =
    case selectionToCollectibleIndices gm.selection of
        Nothing ->
            InvalidMove

        Just collectibleIndices ->
            let
                ( ( ct, nextGrid ), nextRandom ) =
                    Random.step (collectAndGenerateNextGrid collectibleIndices gm.grid) gm.random

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
                    , grid = nextGrid
                    , selectionStack = []
                    , validIndices = []
                    }

            else if isGameLost then
                GameLost
                    { targetSeeds = nextTargetSeeds
                    , targetWater = nextTargetWater
                    , movesLeft = nextMovesLeft
                    , grid = nextGrid
                    , selectionStack = []
                    , validIndices = []
                    }

            else
                NextState
                    (Model
                        { targetSeeds = nextTargetSeeds
                        , targetWater = nextTargetWater
                        , movesLeft = nextMovesLeft
                        , grid = nextGrid
                        , random = nextRandom
                        , selection = emptySelection
                        }
                    )
