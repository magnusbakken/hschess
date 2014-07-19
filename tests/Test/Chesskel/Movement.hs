{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Movement where

import Assertions
import Chesskel.Board
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit

-- Move legality and promotions.
castling = assertHasLegalMove "O-O" berlin
enPassant = assertHasLegalMove "exd6" ["e4", "Nf6", "e5", "d5"]
promotion = assertHasPiece whiteQueen a8 ["a4", "b5", "axb5", "a6", "bxa6", "Nc6", "a7", "Rb8", "a8=Q"]
cannotCastleWhenLostRights = assertDoesNotHaveLegalMove "O-O" (berlin `addMoves` ["Kf1", "Ng8", "Ke1", "Nf6"])

-- Ambiguous moves.
multipleCandidateKnights = assertSourceCellForMove f3 ["Nf3", "Nf6", "d3", "d6", "Nfd2"]
multipleCandidatePawns = assertSourceCellForMove e4 ["e4", "d5", "c4", "e5", "exd5"]

allMovementTests = [
    testCase "Castling" castling,
    testCase "EnPassant" enPassant,
    testCase "Promotion" promotion,
    testCase "CannotCastleWhenLostRights" cannotCastleWhenLostRights,
    testCase "MultipleCandidateKnights" multipleCandidateKnights,
    testCase "MultipleCandidatePawns" multipleCandidatePawns]
