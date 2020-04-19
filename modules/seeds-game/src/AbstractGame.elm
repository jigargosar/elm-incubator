module AbstractGame exposing (GameModel, Info, info, initGame, makeMove)

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


makeMove : Int -> GameModel -> GameModel
makeMove i (GM g) =
    GM { g | currentTarget = g.currentTarget - i, movesLeft = g.movesLeft - 1 }



--type GameOverModel
--    = GameOverModel
--
--
--type Move
--    = ValidMove
--    | InvalidMove
--    | GameOverMove
--
--
--type MoveResult
--    = MoveResultGameOver GameOverModel
--    | MoveResultNextState GameModel
--    | MoveResultInvalidMove
--
--
--makeMove : Move -> GameModel -> MoveResult
--makeMove =
--    Debug.todo "impl"
