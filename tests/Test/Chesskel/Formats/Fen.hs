{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Formats.Fen where

import Chesskel.Board
import Chesskel.Formats.Fen
import Chesskel.Gameplay
import Openings
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testFen gc fen = writeFen (currentPosition gc) @?= fen
startFen = testFen startStandardGame "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
ruyLopezFen = testFen ruyLopez "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
najdorfFen = testFen najdorf "rnbqkb1r/1p2pppp/p2p1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6"

allFenTests = [
    testCase "StartFen" startFen,
    testCase "RuyLopezFen" ruyLopezFen,
    testCase "NajdorfFen" najdorfFen]
