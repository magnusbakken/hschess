{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Gameplay (
    allGameplayTests
) where

import Assertions
import Chesskel.Board
import Chesskel.Gameplay
import ChessTestUtils
import Openings
import QuickCheckInstances
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck hiding (Result)

assertResult :: TestableGame g => Result -> g -> Assertion
assertResult result g = gameResult (game g) @?= result

whiteWinWhenBlackIsCheckmated = assertResult WhiteWin scholarsMate
blackWinWhenWhiteIsCheckmated = assertResult BlackWin foolsMate
drawWhenStalemate = assertResult Draw fastestPossibleStalemate

cannotPlayMoveWhenGameDrawn = assertDoesNotHaveLegalMove "e4" (makeDraw start)
cannotPlayMoveWhenGameWon = assertDoesNotHaveLegalMove "e4" (resign Black start)

gameplayTests = [
    testCase "WhiteWinWhenBlackIsCheckmated" whiteWinWhenBlackIsCheckmated,
    testCase "BlackWinWhenWhiteIsCheckmated" blackWinWhenWhiteIsCheckmated,
    testCase "DrawWhenStalemate" drawWhenStalemate,
    testCase "CannotPlayMoveWhenGameDrawn" cannotPlayMoveWhenGameDrawn,
    testCase "CannotPlayMoveWhenGameWon" cannotPlayMoveWhenGameWon]

prop_moveCountMatchesNumberOfMoves = do
    moveNum <- choose (1 :: Int, 50)
    gc <- arbitraryGameContext moveNum
    return $ moveCount (currentPosition gc) == (length (moves gc) `div` 2) + 1

gameplayProps = [
    testProperty "MoveCountMatchesNumberOfMoves" prop_moveCountMatchesNumberOfMoves]

allGameplayTests = [
    testGroup "Unit tests" gameplayTests,
    testGroup "Invariant properties" gameplayProps]
