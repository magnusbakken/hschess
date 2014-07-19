module ChessTestUtils (
    forcePosition,
    forcePositionFrom,
    forceGame,
    forceGameFrom,
    forcePlaySanMove,
    isLegalSanMove
) where

import Chesskel.Board
import Chesskel.Formats.San
import Chesskel.Gameplay
import Chesskel.Movement

data PlaySanMoveError = SyntaxError SanMoveError | SemanticError MoveError deriving (Eq, Show)

getMoveErrorDescription :: GameContext -> String -> PlaySanMoveError -> String
getMoveErrorDescription gc move err =
    let pos = currentPosition gc
        moveNum = moveCount pos
        dots = if currentPlayer pos == White then "." else "..." in
    show moveNum ++ dots ++ move ++ ": " ++ show err

mapError :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapError f (Left err) = Left $ f err
mapError _ (Right a) = Right a

mapSanError :: Either SanMoveError a -> Either PlaySanMoveError a
mapSanError = mapError SyntaxError

mapMoveError :: Either MoveError a -> Either PlaySanMoveError a
mapMoveError = mapError SemanticError

startGameFrom :: PositionContext -> GameContext
startGameFrom pc = startGame pc (unknownHeaderData, [])

playSanMove :: GameContext -> String -> Either PlaySanMoveError GameContext
playSanMove gc move = do
    unspecMove <- mapSanError (readSanMove move)
    mapMoveError $ playMinimalMove (unspecMove, Nothing) gc

forcePlaySanMove :: GameContext -> String -> GameContext
forcePlaySanMove gc move =
    case playSanMove gc move of
        Left err -> error $ getMoveErrorDescription gc move err
        Right gc' -> gc'

isLegalSanMove :: GameContext -> String -> Bool
isLegalSanMove gc move =
    case playSanMove gc move of
        Left _ -> False
        Right _ -> True

forcePosition :: [String] -> PositionContext
forcePosition = forcePositionFrom startPosition

forcePositionFrom :: PositionContext -> [String] -> PositionContext
forcePositionFrom pc moves = currentPosition $ forceGameFrom (startGameFrom pc) moves

forceGame :: [String] -> GameContext
forceGame = forceGameFrom startStandardGame

forceGameFrom :: GameContext -> [String] -> GameContext
forceGameFrom = foldl forcePlaySanMove
