module AbstractGame exposing (GameModel, Info, MoveResult(..), info, initGame, makeMove)

-- GAME MODEL


type GameModel
    = GM { movesLeft : Int, currentTarget : Int }


initGame : GameModel
initGame =
    GM { movesLeft = 5, currentTarget = 100 }


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
            { g
                | currentTarget = 0
                , movesLeft = g.movesLeft - 1
            }

    else if g.movesLeft == 1 then
        GameLost
            { g
                | currentTarget = g.currentTarget - i
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
