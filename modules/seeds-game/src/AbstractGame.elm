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



-- GAME MODEL


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


isCellMovable : Cell -> Bool
isCellMovable cell =
    case cell of
        Water ->
            True

        Wall ->
            False

        Empty ->
            False



--collectMove : List GI -> GameModel -> GameModel
--collectMove list (GM gm) =
--    let
--        collectedGrid =
--            Grid.map
--                (\i c ->
--                    if List.member i list then
--                        Empty
--
--                    else
--                        c
--                )
--                gm.grid
--
--        ( fallenIndices, fallenGrid ) =
--            computeFallingIndicesAndUpdateGrid collectedGrid
--    in
--    GM
--        { gm
--            | grid = fallenGrid
--            , fallen = fallenIndices
--            , collected = list
--        }


makeMove : List GI -> GameModel -> MoveResult
makeMove list (GM gm) =
    if list == [] then
        InvalidMove

    else
        let
            ( collectedIndices, collectedGrid ) =
                ( list
                , Grid.map
                    (\i c ->
                        if List.member i list then
                            Empty

                        else
                            c
                    )
                    gm.grid
                )

            collectedCount =
                List.length collectedIndices

            ( fallenIndices, fallenGrid ) =
                computeFallingIndicesAndUpdateGrid collectedGrid
        in
        if gm.currentTarget - collectedCount <= 0 then
            GameWon
                { currentTarget = 0
                , movesLeft = gm.movesLeft - 1
                , cells = Grid.toList fallenGrid
                , fallen = fallenIndices
                , collected = collectedIndices
                }

        else if gm.movesLeft == 1 then
            GameLost
                { currentTarget = gm.currentTarget - collectedCount
                , movesLeft = 0
                , cells = Grid.toList fallenGrid
                , fallen = fallenIndices
                , collected = collectedIndices
                }

        else
            NextState
                (GM
                    { currentTarget = gm.currentTarget - collectedCount
                    , movesLeft = 0
                    , grid = fallenGrid
                    , fallen = fallenIndices
                    , collected = collectedIndices
                    }
                )
