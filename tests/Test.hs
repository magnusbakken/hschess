module Main where

import Test.Chesskel.Formats.Fen (allFenTests)
import Test.Chesskel.Movement (allMovementTests)
import Test.Framework (testGroup, defaultMain)

main :: IO ()
main = defaultMain [
    testGroup "FenTests" allFenTests,
    testGroup "MovementTests" allMovementTests
    ]
