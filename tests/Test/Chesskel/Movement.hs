{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Movement where

import Chesskel.Board hiding (hasPiece)
import Chesskel.Movement
import Chesskel.Gameplay
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit
import Test.HUnit

assertOnPosition :: String -> (PositionContext -> Bool) -> [String] -> Assertion
assertOnPosition failMessage predicate moveList =
    assertBool failMessage $ predicate $ currentPosition $ forceGame moveList

assertIsCheckmate :: [String] -> Assertion
assertIsCheckmate = assertOnPosition "Position is not checkmate" isCheckmate

assertIsStalemate :: [String] -> Assertion
assertIsStalemate = assertOnPosition "Position is not stalemate" isStalemate

hasPiece :: Piece -> Cell -> PositionContext -> Bool
hasPiece piece cell pc = hasPieceOfType piece (position pc) cell

assertHasPiece :: Piece -> Cell -> [String] -> Assertion
assertHasPiece piece cell =
    assertOnPosition ("Piece " ++ show piece ++ " is not on cell " ++ show cell) (hasPiece piece cell) where

assertHasLegalMove :: [String] -> String -> Assertion
assertHasLegalMove = assertHasLegalMoveFrom startStandardGame

assertHasLegalMoveFrom :: GameContext -> [String] -> String -> Assertion
assertHasLegalMoveFrom gc moveList move =
    assertBool ("Move " ++ move ++ " was not legal") (isLegalSanMove (forceGameFrom gc moveList) move)

lastMove :: GameContext -> MoveContext
lastMove = head . reverse . moves

assertSourceCell :: [String] -> String -> Cell -> Assertion
assertSourceCell = assertSourceCellFrom startStandardGame

assertSourceCellFrom :: GameContext -> [String] -> String -> Cell -> Assertion
assertSourceCellFrom gc moveList move cell =
    mainFromCell (lastMove (forceGameFrom gc (moveList ++ [move]))) @?= cell

castling = assertHasLegalMoveFrom ruyLopez ["Nf6"] "O-O"
enPassant = assertHasLegalMove ["e4", "Nf6", "e5", "d5"] "exd6"
checkmate = assertIsCheckmate ["f3", "e6", "g4", "Qh4#"]
stalemate = assertIsStalemate ["c4", "h5", "h4", "a5", "Qa4", "Ra6", "Qxa5", "Rah6", "Qxc7", "f6", "Qxd7", "Kf7", "Qxb7", "Qxd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"]
promotion = assertHasPiece (Queen, White) a8 ["a4", "b5", "axb5", "a6", "bxa6", "Nc6", "a7", "Rb8", "a8=Q"]

multipleCandidateKnights = assertSourceCell ["Nf3", "Nf6", "d3", "d6"] "Nfd2" f3
multipleCandidatePawns = assertSourceCell ["e4", "d5", "c4", "e5"] "exd5" e4

allMovementTests = [
    testCase "Castling" castling,
    testCase "EnPassant" enPassant,
    testCase "Checkmate" checkmate,
    testCase "Stalemate" stalemate,
    testCase "Promotion" promotion,
    testCase "MultipleCandidateKnights" multipleCandidateKnights,
    testCase "MultipleCandidatePawns" multipleCandidatePawns]
