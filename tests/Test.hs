module Main where

import Test.Chesskel.Formats.Fen (allFenTests)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

main :: IO ()
main = defaultMain $ hUnitTestToTests allFenTests
