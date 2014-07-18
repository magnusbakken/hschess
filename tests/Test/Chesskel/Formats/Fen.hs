{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Formats.Fen where

import Chesskel.Formats.Fen
import Chesskel.Board
import Openings
import Test.HUnit

testFen pc fen = writeFen pc ~?= fen
testStartFen = testFen startPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
testRuyLopezFen = testFen ruyLopez "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
testNajdorfFen = testFen najdorf "rnbqkb1r/1p2pppp/p2p1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6"

allFenTests = TestList [testStartFen, testRuyLopezFen, testNajdorfFen]
