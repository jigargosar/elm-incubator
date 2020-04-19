module AbstractGame exposing (GameModel, currentTarget, initGame, movesLeft)

-- GAME MODEL


type GameModel
    = GameModel


initGame : GameModel
initGame =
    GameModel


movesLeft : GameModel -> Int
movesLeft _ =
    5


currentTarget : GameModel -> Int
currentTarget _ =
    100



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
