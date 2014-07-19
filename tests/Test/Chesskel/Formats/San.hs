{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Formats.San (
    allSanTests
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Formats.San
import Test.Framework.Providers.HUnit
import Test.HUnit

forceReadSanMove :: String -> UnderspecifiedMove
forceReadSanMove san =
    case readSanMove san of
        Left err -> error $ "Invalid SAN: " ++ show err
        Right unspecMove -> unspecMove

sanMoveIsInvalid :: String -> Bool
sanMoveIsInvalid san =
    case readSanMove san of
        Left _ -> True
        Right _ -> False

assertSan :: (Eq a, Show a) => (UnderspecifiedMove -> a) -> String -> a -> Assertion
assertSan f san expected = f (forceReadSanMove san) @?= expected

assertInvalidSan :: String -> Assertion
assertInvalidSan move =
    assertBool ("Move should've been invalid: " ++ move) $ sanMoveIsInvalid move

assertChessman :: String -> Chessman -> Assertion
assertChessman = assertSan knownChessman

assertFromFile :: String -> Maybe File -> Assertion
assertFromFile = assertSan knownFromFile

assertFromRank :: String -> Maybe Rank -> Assertion
assertFromRank = assertSan knownFromRank

assertIsCapture :: String -> Bool -> Assertion
assertIsCapture = assertSan knownIsCapture

assertToCell :: String -> Cell -> Assertion
assertToCell = assertSan knownToCell

assertPromotionTarget :: String -> Maybe PromotionTarget -> Assertion
assertPromotionTarget = assertSan knownPromotionTarget

assertCheckState :: String -> Maybe CheckState -> Assertion
assertCheckState = assertSan knownCheckState

assertCastling :: String -> CastlingDirection -> Assertion
assertCastling san expectedCastling = getCastling (forceReadSanMove san) @?= Just expectedCastling where
    getCastling (CastleMove actualCastling) = Just actualCastling
    getCastling _ = Nothing

pawn = assertChessman "c3" Pawn
oldSchoolPawn = assertChessman "Pc3" Pawn
knight = assertChessman "Na3" Knight
bishop = assertChessman "Bb4" Bishop
rook = assertChessman "Rg8" Rook
queen = assertChessman "Qf7" Queen
king = assertChessman "Kd2" King

withPawnSourceFile = assertFromFile "dxc5" (Just FileD)
withoutPawnSourceFile = assertFromFile "c5" Nothing
withQueenSourceFile = assertFromFile "Qab1" (Just FileA)
withoutQueenSourceFile = assertFromFile "Qb1" Nothing
withQueenSourceRank = assertFromRank "Q6g5" (Just Rank6)
withoutQueenSourceRank = assertFromRank "Qg5" Nothing

pawnCapture = assertIsCapture "gxf6" True
pawnNonCapture = assertIsCapture "f6" False
kingCapture = assertIsCapture "Kxf1" True
kingNonCapture = assertIsCapture "Kf1" False

pawnMoveToCell = assertToCell "e4" e4
knightMoveToCell = assertToCell "Nf3" f3
wackyMoveToCell = assertToCell "Rh3h2+" h2

promotionTest = assertPromotionTarget "f8=N" (Just PKnight)
nonPromotionTest = assertPromotionTarget "f7" Nothing

nonCheck = assertCheckState "Bb2" Nothing
check = assertCheckState "Bb2+" (Just Check)
checkmate = assertCheckState "Bb2#" (Just Checkmate)

castleKingside = assertCastling "O-O" Kingside
castleQueenside = assertCastling "O-O-O" Queenside
castleKingsideWithZeroes = assertCastling "0-0" Kingside
castleQueensideWithZeroes = assertCastling "0-0-0" Queenside

invalidPawnCapture = assertInvalidSan "ed4"
invalidPromotion = assertInvalidSan "e4Q"
invalidNonPawnPromotion = assertInvalidSan "Qe8=N"

allSanTests = [
    testCase "Pawn" pawn,
    testCase "OldSchoolPawn" oldSchoolPawn,
    testCase "Knight" knight,
    testCase "Bishop" bishop,
    testCase "Rook" rook,
    testCase "Queen" queen,
    testCase "King" king,
    testCase "WithPawnSourceFile" withPawnSourceFile,
    testCase "WithoutPawnSourceFile" withoutPawnSourceFile,
    testCase "WithQueenSourceFile" withQueenSourceFile,
    testCase "WithoutQueenSourceFile" withoutQueenSourceFile,
    testCase "WithQueenSourceRank" withQueenSourceRank,
    testCase "WithoutQueenSourceRank" withoutQueenSourceRank,
    testCase "PawnMoveToCell" pawnMoveToCell,
    testCase "KnightMoveToCell" knightMoveToCell,
    testCase "WackyMoveToCell" wackyMoveToCell,
    testCase "PawnCapture" pawnCapture,
    testCase "PawnNonCapture" pawnNonCapture,
    testCase "KingCapture" kingCapture,
    testCase "KingNonCapture" kingNonCapture,
    testCase "PromotionTest" promotionTest,
    testCase "NonPromotionTest" nonPromotionTest,
    testCase "NonCheck" nonCheck,
    testCase "Check" check,
    testCase "Checkmate" checkmate,
    testCase "CastleKingside" castleKingside,
    testCase "CastleQueenside" castleQueenside,
    testCase "CastleKingsideWithZeroes" castleKingsideWithZeroes,
    testCase "CastleQueensideWithZeroes" castleQueensideWithZeroes,
    testCase "InvalidPawnCapture" invalidPawnCapture,
    testCase "InvalidPromotion" invalidPromotion,
    testCase "InvalidNonPawnPromotion" invalidNonPawnPromotion]
