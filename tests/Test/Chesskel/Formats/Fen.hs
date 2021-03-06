{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Chesskel.Formats.Fen (
    allFenTests
) where

import Chesskel.Board
import Chesskel.Formats.Fen
import Chesskel.Gameplay
import Openings
import Test.Framework.Providers.HUnit
import Test.HUnit

forceReadFen :: String -> PositionContext
forceReadFen fen =
    case readFen fen of
        Left err -> error $ "Error in FEN: " ++ show err
        Right pc -> pc

hasSyntaxError :: String -> Bool
hasSyntaxError fen =
    case readFen fen of
        Left (FenSyntaxError _) -> True
        _ -> False

hasSemanticError :: String -> Bool
hasSemanticError fen =
    case readFen fen of
        Left (FenPositionError _) -> True
        _ -> False

assertSyntaxError :: String -> Assertion
assertSyntaxError = assertBool "Should've been FenSyntaxError" . hasSyntaxError

assertSemanticError :: String -> Assertion
assertSemanticError = assertBool "Should've been FenPositionError" . hasSemanticError

assertWriteFen :: GameContext -> String -> Assertion
assertWriteFen gc fen = writeFen (currentPosition gc) @?= fen

assertReadFen :: String -> GameContext -> Assertion
assertReadFen fen gc = forceReadFen fen @?= currentPosition gc

writeStartFen = assertWriteFen startStandardGame "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
writeRuyLopezFen = assertWriteFen ruyLopez "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
writeNajdorfFen = assertWriteFen najdorf "rnbqkb1r/1p2pppp/p2p1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6"

readStartFen = assertReadFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" startStandardGame
readRuyLopezFen = assertReadFen "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3" ruyLopez
readNajdorfFen = assertReadFen "rnbqkb1r/1p2pppp/p2p1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6" najdorf

missingRow = assertSyntaxError "rnbqkbnr/pppppppp/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
extraRow = assertSyntaxError "rnbqkbnr/pppppppp/8/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
missingCell = assertSemanticError "rnbqkbnr/pppppppp/7/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
extraCell = assertSemanticError "rnbqkbnr/pppppppp/4P4/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

allFenTests = [
    testCase "WriteStartFen" writeStartFen,
    testCase "WriteRuyLopezFen" writeRuyLopezFen,
    testCase "WriteNajdorfFen" writeNajdorfFen,
    testCase "ReadStartFen" readStartFen,
    testCase "ReadRuyLopezFen" readRuyLopezFen,
    testCase "ReadNajdorfFen" readNajdorfFen,
    testCase "MissingRow" missingRow,
    testCase "ExtraRow" extraRow,
    testCase "MissingCell" missingCell,
    testCase "ExtraCell" extraCell]
