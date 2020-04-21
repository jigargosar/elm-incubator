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

import Basics.Extra exposing (swap)
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
        , currentTarget : Int
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
        , currentTarget = 100
        , grid = initialGrid
        }


type alias Info =
    { movesLeft : Int
    , currentTarget : Int
    , grid : Grid Cell
    }


info : GameModel -> Info
info (GM g) =
    Info g.movesLeft g.currentTarget g.grid


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


isCellCollectible : Cell -> Bool
isCellCollectible cell =
    case cell of
        Water ->
            True

        Seed ->
            True

        Wall ->
            False

        Empty ->
            False


collectIndices : List GI -> Grid Cell -> ( Int, Grid Cell )
collectIndices indicesToCollect grid0 =
    let
        computeCollectedAt idx ( ct, grid ) =
            case Grid.get idx grid of
                Just cell ->
                    if isCellCollectible cell then
                        case Grid.set idx Empty grid of
                            Just collectedGrid ->
                                Just ( ct + 1, collectedGrid )

                            Nothing ->
                                Nothing

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    filterFoldr computeCollectedAt ( 0, grid0 ) indicesToCollect


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
            ( collectedCount, collectedGrid_ ) =
                collectIndices input gm.grid

            ( _, fallenGrid_ ) =
                computeFallingIndicesAndUpdateGrid collectedGrid_

            filledGrid =
                fillEmptyCells fallenGrid_

            nextTarget =
                gm.currentTarget - collectedCount

            nextMovesLeft =
                gm.movesLeft - 1
        in
        if nextTarget <= 0 then
            GameWon
                { currentTarget = 0
                , movesLeft = nextMovesLeft
                , grid = filledGrid
                }

        else if gm.movesLeft == 1 then
            GameLost
                { currentTarget = nextTarget
                , movesLeft = 0
                , grid = filledGrid
                }

        else
            NextState
                (GM
                    { currentTarget = nextTarget
                    , movesLeft = nextMovesLeft
                    , grid = filledGrid
                    }
                )
