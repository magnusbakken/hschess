module Assertions (
    assertOnPosition,
    assertIsCheckmate,
    assertIsStalemate,
    assertHasPiece,
    assertHasNoPiece,
    assertHasLegalMove,
    assertDoesNotHaveLegalMove,
    assertSourceCellForMove,
    assertMoveError
) where

import Chesskel.Board hiding (hasPiece)
import Chesskel.Movement
import Chesskel.Gameplay
import ChessTestUtils
import Data.Maybe
import Test.HUnit

assertOnPosition :: TestableGame g => String -> (PositionContext -> Bool) -> g -> Assertion
assertOnPosition failMessage predicate g = assertBool failMessage (predicate (currentPosition (game g)))

assertIsCheckmate :: TestableGame g => g -> Assertion
assertIsCheckmate = assertOnPosition "Position is not checkmate" isCheckmate

assertIsStalemate :: TestableGame g => g -> Assertion
assertIsStalemate = assertOnPosition "Position is not stalemate" isStalemate

hasPiece :: Piece -> Cell -> PositionContext -> Bool
hasPiece piece cell pc = hasPieceOfType piece (position pc) cell

hasNoPiece :: Cell -> PositionContext -> Bool
hasNoPiece cell pc = isNothing $ getSquare (position pc) cell

assertHasPiece :: TestableGame g => Piece -> Cell -> g -> Assertion
assertHasPiece piece cell = assertOnPosition failMessage (hasPiece piece cell) where
    failMessage = "Piece " ++ show piece ++ " is not on cell " ++ show cell

assertHasNoPiece :: TestableGame g => Cell -> g -> Assertion
assertHasNoPiece cell = assertOnPosition failMessage (hasNoPiece cell) where
    failMessage = "Cell " ++ show cell ++ " is not empty"

assertHasLegalMove :: TestableGame g => String -> g -> Assertion
assertHasLegalMove move g = assertBool failMessage (canMove g move) where
    failMessage = "Move " ++ move ++ " is not legal"

assertDoesNotHaveLegalMove :: TestableGame g => String -> g -> Assertion
assertDoesNotHaveLegalMove move g = assertBool failMessage (not $ canMove g move) where
    failMessage = "Move " ++ move ++ " is legal"

assertSourceCellForMove :: TestableGame g => Cell -> g -> Assertion
assertSourceCellForMove cell g = mainFromCell (lastMove (game g)) @?= cell

assertMoveError :: TestableGame g => MoveError -> String -> g -> Assertion
assertMoveError expectedError move g = getMoveError g move @?= Just expectedError
