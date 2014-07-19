{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Board (
    allBoardTests
) where

import Assertions
import Chesskel.Board
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit

checkmate = assertIsCheckmate foolsMate
stalemate = assertIsStalemate fastestPossibleStalemate
startPositionHasKing = assertHasPiece whiteKing e1 start
rookIsMovedWhenCastling = assertHasPiece whiteRook f1 (berlin `addMove` "O-O")
capturedPieceIsReplaced = assertHasPiece whitePawn d5 (scandinavian `addMove` "exd5")
pawnDisappearsAfterEnPassantCapture = assertHasNoPiece d5 ["e4", "Nf6", "e5", "d5", "exd6"]

allBoardTests = [
    testCase "Checkmate" checkmate,
    testCase "Stalemate" stalemate,
    testCase "StartPositionHasKing" startPositionHasKing,
    testCase "RookIsMovedWhenCastling" rookIsMovedWhenCastling,
    testCase "CapturedPieceIsReplaced" capturedPieceIsReplaced,
    testCase "PawnDisappearsAfterEnPassantCapture" pawnDisappearsAfterEnPassantCapture]
