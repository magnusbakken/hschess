module Main where

import Test.Chesskel.Board (allBoardTests)
import Test.Chesskel.Formats.Fen (allFenTests)
import Test.Chesskel.Movement (allMovementTests)
import Test.Framework (testGroup, defaultMain)

main :: IO ()
main = defaultMain [
    testGroup "BoardTests" allBoardTests,
    testGroup "MovementTests" allMovementTests,
    testGroup "FenTests" allFenTests]
