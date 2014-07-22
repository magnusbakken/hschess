{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Movement (
    allMovementTests
) where

import Assertions
import Chesskel.Board
import Chesskel.Movement
import ChessTestUtils
import Openings
import QuickCheckInstances
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- Move legality and promotions.
castling = assertHasLegalMove "O-O" berlin
enPassant = assertHasLegalMove "exd6" ["e4", "Nf6", "e5", "d5"]
promotion = assertHasPiece whiteQueen a8 ["a4", "b5", "axb5", "a6", "bxa6", "Nc6", "a7", "Rb8", "a8=Q"]
cannotCastleWhenLostRights = assertDoesNotHaveLegalMove "O-O" (berlin `addMoves` ["Kf1", "Ng8", "Ke1", "Nf6"]) 

-- Ambiguous moves.
multipleCandidateKnights = assertSourceCellForMove f3 ["Nf3", "Nf6", "d3", "d6", "Nfd2"]
multipleCandidatePawns = assertSourceCellForMove e4 ["e4", "d5", "c4", "e5", "exd5"]

-- Illegal moves.
noPieceAtSourceSquare = assertMoveError NoPieceAtSourceSquare "Ra2a3" start
pieceCannotReachSquare = assertMoveError PieceCannotReachSquare "Nb1a4" start
moveWouldLeaveKingInCheck = assertMoveError MoveWouldLeaveKingInCheck "Nc3e4" ["d4", "e6", "Nc3", "Bb4"]
promotionIsNeeded = assertMoveError PromotionIsNeeded "a8" ["b4", "a5", "bxa5", "Nc6", "a6", "Rb8", "a7", "Na5"]
promotionIsNotNeeded = assertMoveError PromotionIsNotNeeded "e4=Q" start
doesNotHaveCastlingRights = assertMoveError (DoesNotHaveCastlingRights (Kingside, White)) "O-O" (berlin `addMoves` ["Kf1", "Ng8", "Ke1", "Nf6"])
castlingIsNotPossible = assertMoveError (CastlingIsNotPossible (Kingside, White)) "O-O" ["Nf3", "d6", "g3", "Nc6", "Bh3", "Bxh3"]
insufficientDisambiguation = assertMoveError (InsufficientDisambiguation [b1, f3]) "Nd2" ["Nf3", "Nf6", "d3", "d6"]

movementTests = [
    testCase "Castling" castling,
    testCase "EnPassant" enPassant,
    testCase "Promotion" promotion,
    testCase "CannotCastleWhenLostRights" cannotCastleWhenLostRights,
    testCase "MultipleCandidateKnights" multipleCandidateKnights,
    testCase "MultipleCandidatePawns" multipleCandidatePawns,
    testCase "NoPieceAtSourceSquare" noPieceAtSourceSquare,
    testCase "PieceCannotReachSquare" pieceCannotReachSquare,
    testCase "MoveWouldLeaveKingInCheck" moveWouldLeaveKingInCheck,
    testCase "PromotionIsNeeded" promotionIsNeeded,
    testCase "PromotionIsNotNeeded" promotionIsNotNeeded,
    testCase "DoesNotHaveCastlingRights" doesNotHaveCastlingRights,
    testCase "CastlingIsNotPossible" castlingIsNotPossible,
    testCase "InsufficientDisambiguation" insufficientDisambiguation]

extractMove :: MoveContext -> Move
extractMove mc = createMove (mainFromCell mc) (mainToCell mc)

-- Move invariants.
prop_IsLegalMoveIsTrueForMoveFromFindAllLegalMoves = do
    moveNum <- choose (1 :: Int, 10)
    (mc, pc) <- arbitraryMoveForPosition moveNum
    return (isLegalMove pc (extractMove mc) (promotionTarget mc))

prop_IsLegalMoveIsFalseForMoveNotFromFindAllLegalMoves = do
    moveNum <- choose (1 :: Int, 10)
    pc <- arbitraryPositionContext moveNum
    move <- arbitrary
    move `notElem` map extractMove (findAllLegalMoves pc) ==>
        not (isLegalMove pc move Nothing)

movementProps = [
    testProperty "IsLegalMoveIsTrueForMoveFromFindAllLegalMoves" prop_IsLegalMoveIsTrueForMoveFromFindAllLegalMoves,
    testProperty "IsLegalMoveIsFalseForMoveNotFromFindAllLegalMoves" prop_IsLegalMoveIsFalseForMoveNotFromFindAllLegalMoves]

allMovementTests = [
    testGroup "Unit tests" movementTests,
    testGroup "Invariant properties" movementProps]
