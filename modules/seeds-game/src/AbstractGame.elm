module AbstractGame exposing
    ( Cell(..)
    , GameModel
    , Info
    , MoveResult(..)
    , info
    , initGame
    , makeMove
    )

-- GAME GRID

import Basics.Extra exposing (atLeast, swap)
import Grid exposing (GI, Grid)
import List.Extra


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
        { movesLeft = 5
        , targetSeeds = 50
        , targetWater = 50
        , grid = initialGrid
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


fillEmptyCells : Grid Cell -> Grid Cell
fillEmptyCells =
    Grid.map
        (\_ c ->
            if c == Empty then
                Water

            else
                c
        )


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

            filledGrid =
                fillEmptyCells fallenGrid_

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
                    }
                )
