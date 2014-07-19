{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Movement where

import Chesskel.Board hiding (hasPiece)
import Chesskel.Movement
import Chesskel.Gameplay
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit
import Test.HUnit

assertOnPosition :: TestableGame g => String -> (PositionContext -> Bool) -> g -> Assertion
assertOnPosition failMessage predicate g = assertBool failMessage (predicate (currentPosition (game g)))

assertIsCheckmate :: TestableGame g => g -> Assertion
assertIsCheckmate = assertOnPosition "Position is not checkmate" isCheckmate

assertIsStalemate :: TestableGame g => g -> Assertion
assertIsStalemate = assertOnPosition "Position is not stalemate" isStalemate

hasPiece :: Piece -> Cell -> PositionContext -> Bool
hasPiece piece cell pc = hasPieceOfType piece (position pc) cell

assertHasPiece :: TestableGame g => Piece -> Cell -> g -> Assertion
assertHasPiece piece cell = assertOnPosition failMessage (hasPiece piece cell) where
    failMessage = "Piece " ++ show piece ++ " is not on cell " ++ show cell

assertHasLegalMove :: TestableGame g => String -> g -> Assertion
assertHasLegalMove move g = assertBool failMessage (canMove g move) where
    failMessage = "Move " ++ move ++ " was not legal"

assertSourceCell :: TestableGame g => Cell -> g -> Assertion
assertSourceCell cell g = mainFromCell (lastMove (game g)) @?= cell

castling = assertHasLegalMove "O-O" berlin
enPassant = assertHasLegalMove "exd6" ["e4", "Nf6", "e5", "d5"]
checkmate = assertIsCheckmate foolsMate
stalemate = assertIsStalemate ["c4", "h5", "h4", "a5", "Qa4", "Ra6", "Qxa5", "Rah6", "Qxc7", "f6", "Qxd7", "Kf7", "Qxb7", "Qxd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"]
promotion = assertHasPiece whiteQueen a8 ["a4", "b5", "axb5", "a6", "bxa6", "Nc6", "a7", "Rb8", "a8=Q"]

multipleCandidateKnights = assertSourceCell f3 ["Nf3", "Nf6", "d3", "d6", "Nfd2"]
multipleCandidatePawns = assertSourceCell e4 ["e4", "d5", "c4", "e5", "exd5"]

allMovementTests = [
    testCase "Castling" castling,
    testCase "EnPassant" enPassant,
    testCase "Checkmate" checkmate,
    testCase "Stalemate" stalemate,
    testCase "Promotion" promotion,
    testCase "MultipleCandidateKnights" multipleCandidateKnights,
    testCase "MultipleCandidatePawns" multipleCandidatePawns]
