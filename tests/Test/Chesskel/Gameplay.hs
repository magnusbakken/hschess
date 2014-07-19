{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Gameplay (
    allGameplayTests
) where

import Assertions
import Chesskel.Board
import Chesskel.Gameplay
import Openings
import Test.Framework.Providers.HUnit

cannotPlayMoveWhenGameDrawn = assertDoesNotHaveLegalMove "e4" (makeDraw start)
cannotPlayMoveWhenGameWon = assertDoesNotHaveLegalMove "e4" (resign Black start)

allGameplayTests = [
    testCase "CannotPlayMoveWhenGameDrawn" cannotPlayMoveWhenGameDrawn,
    testCase "CannotPlayMoveWhenGameWon" cannotPlayMoveWhenGameWon]
