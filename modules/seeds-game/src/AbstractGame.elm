module AbstractGame exposing (Cell(..), GameModel, Info, MoveResult(..), collectIndices, info, initGame, makeMove)

-- GAME MODEL

import Basics.Extra exposing (flip)
import Dict
import Grid exposing (GI, Grid)
import List.Extra


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



--type Next s a
--    = NextSeedAndItem s a
--    | NextSeed s
--    | End
--
--
--listFromSeed : (s -> Next s a) -> s -> List a
--listFromSeed toNext =
--    let
--        func list s0 =
--            case toNext s0 of
--                NextSeedAndItem s a ->
--                    func (a :: list) s
--
--                NextSeed s ->
--                    func list s
--
--                End ->
--                    list
--    in
--    func []
--type Find s a
--    = Continue s
--    | NotFound
--    | Found a
--
--
--find : (s -> Find s a) -> s -> Maybe a
--find func seed0 =
--    case func seed0 of
--        Continue s ->
--            find func s
--
--        NotFound ->
--            Nothing
--
--        Found a ->
--            Just a


findFirstMovableAbove : GI -> Grid Cell -> Maybe GI
findFirstMovableAbove srcIndex grid =
    case Grid.entryAbove srcIndex grid of
        Nothing ->
            Nothing

        Just ( nextIndex, cell ) ->
            if isCellMovable cell then
                Just nextIndex

            else
                findFirstMovableAbove nextIndex grid


isCellMovable : Cell -> Bool
isCellMovable cell =
    case cell of
        Water ->
            True

        Wall ->
            False

        Empty ->
            False


lastEmpty : Grid Cell -> Maybe GI
lastEmpty grid =
    Grid.toDict grid
        |> Dict.filter (\_ cell -> cell == Empty)
        |> Dict.keys
        |> List.maximum


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


computeFallingIndexPairs : Grid Cell -> List ( GI, GI )
computeFallingIndexPairs =
    List.Extra.unfoldr
        (\grid ->
            findLastFallingIndexPair grid
                |> Maybe.andThen
                    (\( si, ei ) ->
                        case Grid.swap si ei grid of
                            Nothing ->
                                Nothing

                            Just swapped ->
                                Just ( ( si, ei ), swapped )
                    )
        )


findLastFallingIndexPair : Grid Cell -> Maybe ( GI, GI )
findLastFallingIndexPair grid =
    lastEmpty grid
        |> Maybe.andThen
            (\ei ->
                findFirstMovableAbove ei grid
                    |> Maybe.map (\si -> ( si, ei ))
            )


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func val =
    case func val of
        Just nextVal ->
            applyWhileJust func nextVal

        Nothing ->
            val


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
