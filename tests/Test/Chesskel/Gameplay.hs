{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Gameplay (
    allGameplayTests
) where

import Assertions
import Chesskel.Board
import Chesskel.Gameplay
import ChessTestUtils
import Openings
import Test.Framework.Providers.HUnit
import Test.HUnit

assertResult :: TestableGame g => Result -> g -> Assertion
assertResult result g = gameResult (game g) @?= result

whiteWinWhenBlackIsCheckmated = assertResult WhiteWin scholarsMate
blackWinWhenWhiteIsCheckmated = assertResult BlackWin foolsMate
drawWhenStalemate = assertResult Draw fastestPossibleStalemate

cannotPlayMoveWhenGameDrawn = assertDoesNotHaveLegalMove "e4" (makeDraw start)
cannotPlayMoveWhenGameWon = assertDoesNotHaveLegalMove "e4" (resign Black start)

allGameplayTests = [
    testCase "WhiteWinWhenBlackIsCheckmated" whiteWinWhenBlackIsCheckmated,
    testCase "BlackWinWhenWhiteIsCheckmated" blackWinWhenWhiteIsCheckmated,
    testCase "DrawWhenStalemate" drawWhenStalemate,
    testCase "CannotPlayMoveWhenGameDrawn" cannotPlayMoveWhenGameDrawn,
    testCase "CannotPlayMoveWhenGameWon" cannotPlayMoveWhenGameWon]
