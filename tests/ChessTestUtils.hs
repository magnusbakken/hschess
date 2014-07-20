{-# LANGUAGE FlexibleInstances #-}
module ChessTestUtils (
    TestableGame (..)
) where

import Chesskel.Board
import Chesskel.Formats.San
import Chesskel.Gameplay
import Chesskel.Movement

-- |This type class lets you treat lists of strings and GameContext objects in
--  a similar manner for the purposes of testing. Lists of strings are treated
--  as consecutive SAN moves, and each move will be played in a \"forced\"
--  manner, with move validation faults being turned into errors. This means
--  that when using this class with lists of strings, everything you do could
--  potentially result in an error. Even when using it with actual GameContexts
--  most of the functions (all but game and lastPosition) are partial.
class TestableGame g where
    game :: g -> GameContext
    
    addMove :: g -> String -> g
    
    addMoves :: g -> [String] -> g
    addMoves = foldl addMove
    
    lastPosition :: g -> PositionContext
    lastPosition = currentPosition . game
    
    lastMove :: g -> MoveContext
    lastMove = head . reverse . moves . game
    
    canMove :: g -> String -> Bool
    canMove g move =
        case playSanMove (game g) move of
            Left _ -> False
            Right _ -> True

instance TestableGame GameContext where
    game = id
    addMove g move =
        either (error . getMoveErrorDescription g move) id (playSanMove g move)

instance TestableGame [String] where
    game = addMoves startStandardGame
    addMove g move = g ++ [move]

data PlaySanMoveError = SyntaxError SanMoveError | SemanticError MoveError
    deriving (Eq, Show)

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

playSanMove :: GameContext -> String -> Either PlaySanMoveError GameContext
playSanMove gc move = do
    unspecMove <- mapSanError (readSanMove move)
    mapMoveError $ playMinimalMove (unspecMove, Nothing) gc
