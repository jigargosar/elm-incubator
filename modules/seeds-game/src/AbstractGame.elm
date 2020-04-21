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


flipMapAccumr : (a -> c -> ( b, c )) -> c -> List a -> ( List b, c )
flipMapAccumr func acc =
    List.Extra.mapAccumr (\a b -> func b a |> swap) acc
        >> swap


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


type GameModel
    = GM
        { movesLeft : Int
        , currentTarget : Int
        , grid : Grid Cell
        , fallen : List ( GI, GI )
        , collected : List GI
        }


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
    GM
        { movesLeft = 5
        , currentTarget = 100
        , grid = initialGrid
        , fallen = []
        , collected = []
        }


type alias Info =
    { movesLeft : Int
    , currentTarget : Int
    , cells : List ( GI, Cell )
    , fallen : List ( GI, GI )
    , collected : List GI
    }


info : GameModel -> Info
info (GM g) =
    Info g.movesLeft g.currentTarget (Grid.toList g.grid) g.fallen g.collected


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
        collectedGrid =
            Grid.map
                (\i c ->
                    if List.member i list then
                        Empty

                    else
                        c
                )
                gm.grid

        ( fallenIndices, fallenGrid ) =
            computeFallingIndicesAndUpdateGrid collectedGrid

        _ =
            Debug.log "fallenIndices" fallenIndices
    in
    GM
        { gm
            | grid = fallenGrid
            , fallen = fallenIndices
            , collected = list
        }


makeMove : Int -> GameModel -> MoveResult
makeMove i (GM g) =
    if i <= 0 then
        InvalidMove

    else if g.currentTarget - i <= 0 then
        GameWon
            { currentTarget = 0
            , movesLeft = g.movesLeft - 1
            , cells = Grid.toList g.grid
            , fallen = []
            , collected = []
            }

    else if g.movesLeft == 1 then
        GameLost
            { currentTarget = g.currentTarget - i
            , movesLeft = 0
            , cells = Grid.toList g.grid
            , fallen = []
            , collected = []
            }

    else
        NextState
            (GM
                { g
                    | currentTarget = g.currentTarget - i
                    , movesLeft = g.movesLeft - 1
                }
            )
