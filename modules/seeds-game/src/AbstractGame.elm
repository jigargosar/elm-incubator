module AbstractGame exposing (GameModel, Info, MoveResult(..), info, initGame, makeMove)

-- GAME MODEL

import Grid exposing (Grid)


type GameModel
    = GM { movesLeft : Int, currentTarget : Int, grid : Grid Cell }


type Cell
    = Water
    | Wall
    | Seed


initialGrid =
    let
        wallIndices =
            [ ( 2, 1 ), ( 4, 1 ), ( 2, 3 ), ( 4, 3 ) ]

        grid =
            Grid.init
                7
                5
                (\i ->
                    if List.member i wallIndices then
                        Wall

                    else
                        Water
                )
    in
    grid


initGame : GameModel
initGame =
    GM { movesLeft = 5, currentTarget = 100, grid = initialGrid }


type alias Info =
    { movesLeft : Int, currentTarget : Int }


info : GameModel -> Info
info (GM g) =
    Info g.movesLeft g.currentTarget


type MoveResult
    = InvalidMove
    | GameLost Info
    | GameWon Info
    | NextState GameModel


makeMove : Int -> GameModel -> MoveResult
makeMove i (GM g) =
    if i <= 0 then
        InvalidMove

    else if g.currentTarget - i <= 0 then
        GameWon
            { currentTarget = 0
            , movesLeft = g.movesLeft - 1
            }

    else if g.movesLeft == 1 then
        GameLost
            { currentTarget = g.currentTarget - i
            , movesLeft = 0
            }

    else
        NextState
            (GM
                { g
                    | currentTarget = g.currentTarget - i
                    , movesLeft = g.movesLeft - 1
                }
            )
