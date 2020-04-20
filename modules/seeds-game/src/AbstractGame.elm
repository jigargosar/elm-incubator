module AbstractGame exposing
    ( Cell(..)
    , GameModel
    , Info
    , MoveResult(..)
    , collectIndices
    , info
    , initGame
    , makeMove
    )

-- GAME MODEL

import Grid exposing (GI, Grid)


type alias FallingAcc =
    { pendingIndices : List GI
    , fallenIndexPairs : List ( GI, GI )
    , grid : Grid Cell
    }


type alias FallingEntry =
    ( ( GI, GI ), Grid Cell )


computeFallingAt : GI -> Grid Cell -> Maybe FallingEntry
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


computeFalling : FallingAcc -> ( List ( GI, GI ), Grid Cell )
computeFalling acc =
    case acc.pendingIndices of
        [] ->
            ( acc.fallenIndexPairs, acc.grid )

        emptyIndex :: remaining ->
            case Grid.get emptyIndex acc.grid of
                Just Empty ->
                    case findFirstMovableAbove emptyIndex acc.grid of
                        Just movableIndex ->
                            case Grid.swap movableIndex emptyIndex acc.grid of
                                Just sg ->
                                    computeFalling
                                        { acc
                                            | grid = sg
                                            , fallenIndexPairs = ( movableIndex, emptyIndex ) :: acc.fallenIndexPairs
                                            , pendingIndices = remaining
                                        }

                                Nothing ->
                                    computeFalling { acc | pendingIndices = remaining }

                        Nothing ->
                            computeFalling { acc | pendingIndices = remaining }

                _ ->
                    computeFalling { acc | pendingIndices = remaining }


type GameModel
    = GM { movesLeft : Int, currentTarget : Int, grid : Grid Cell }


type Cell
    = Water
    | Wall
    | Empty


initialGrid : Grid Cell
initialGrid =
    let
        wallIndices =
            [ ( 2, 1 ), ( 4, 1 ), ( 2, 3 ), ( 4, 3 ) ]

        emptyIndices =
            [ ( 3, 2 ) ]

        grid =
            Grid.init
                7
                5
                (\i ->
                    if List.member i wallIndices then
                        Wall

                    else if List.member i emptyIndices then
                        Empty

                    else
                        Water
                )
    in
    grid


initGame : GameModel
initGame =
    GM { movesLeft = 5, currentTarget = 100, grid = initialGrid }


type alias Info =
    { movesLeft : Int
    , currentTarget : Int
    , cells : List ( GI, Cell )
    }


info : GameModel -> Info
info (GM g) =
    Info g.movesLeft g.currentTarget (Grid.toList g.grid)


type MoveResult
    = InvalidMove
    | GameLost Info
    | GameWon Info
    | NextState GameModel


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

        Wall ->
            False

        Empty ->
            False


collectIndices : List GI -> GameModel -> GameModel
collectIndices list (GM gm) =
    let
        makeEmpty i c =
            if List.member i list then
                Empty

            else
                c

        ng =
            Grid.map makeEmpty gm.grid
    in
    GM { gm | grid = ng }


makeMove : Int -> GameModel -> MoveResult
makeMove i (GM g) =
    if i <= 0 then
        InvalidMove

    else if g.currentTarget - i <= 0 then
        GameWon
            { currentTarget = 0
            , movesLeft = g.movesLeft - 1
            , cells = Grid.toList g.grid
            }

    else if g.movesLeft == 1 then
        GameLost
            { currentTarget = g.currentTarget - i
            , movesLeft = 0
            , cells = Grid.toList g.grid
            }

    else
        NextState
            (GM
                { g
                    | currentTarget = g.currentTarget - i
                    , movesLeft = g.movesLeft - 1
                }
            )
