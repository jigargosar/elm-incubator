module AbstractGame exposing (GameModel, initGame, movesRemaining)

-- GAME MODEL


type GameModel
    = GameModel


initGame : GameModel
initGame =
    GameModel


movesRemaining : GameModel -> Int
movesRemaining _ =
    5



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
