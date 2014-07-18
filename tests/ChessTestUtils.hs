module ChessTestUtils (
    playSanMove,
    forceSanMove,
    forceSanMoves,
    forcePosition
) where

import Chesskel.Board
import Chesskel.Formats.San
import Chesskel.Gameplay
import Chesskel.Movement

data PlaySanMoveError = SyntaxError SanMoveError | SemanticError MoveError deriving (Eq, Show)

mapError :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapError f (Left err) = Left $ f err
mapError _ (Right a) = Right a

mapSanError :: Either SanMoveError a -> Either PlaySanMoveError a
mapSanError = mapError SyntaxError

mapMoveError :: Either MoveError a -> Either PlaySanMoveError a
mapMoveError = mapError SemanticError

playSanMove :: GameContext -> String -> Either PlaySanMoveError GameContext
playSanMove gc s = do
    unspecMove <- mapSanError (readSanMove s)
    mapMoveError $ playMinimalMove (unspecMove, Nothing) gc

forceSanMove :: GameContext -> String -> GameContext
forceSanMove gc s =
    case playSanMove gc s of
        Left err -> error $ show err
        Right gc' -> gc'

forceSanMoves :: GameContext -> [String] -> GameContext
forceSanMoves = foldl forceSanMove

forcePosition :: [String] -> PositionContext
forcePosition = currentPosition . forceSanMoves startStandardGame
