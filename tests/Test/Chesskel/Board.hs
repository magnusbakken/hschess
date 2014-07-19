{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Board where

import Assertions
import Chesskel.Board
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit

checkmate = assertIsCheckmate foolsMate
stalemate = assertIsStalemate ["c4", "h5", "h4", "a5", "Qa4", "Ra6", "Qxa5", "Rah6", "Qxc7", "f6", "Qxd7", "Kf7", "Qxb7", "Qxd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"]
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
