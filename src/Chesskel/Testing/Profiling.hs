module Chesskel.Testing.Profiling where

import Chesskel.Formats.Pgn
import System.IO

readPgnFromFile :: FilePath -> IO String
readPgnFromFile name = openFile name ReadMode >>= hGetContents

forceReadPgn :: FilePath -> IO String
forceReadPgn pgnFile = do
    pgnString <- readPgnFromFile pgnFile
    case readPgn pgnString of
        Left err -> error $ "Unable to read game: " ++ show err
        Right game ->
            case writePgn game of
                Left err' -> error $ "Unable to write game: " ++ show err'
                Right pgnString' -> return pgnString'

profile :: FilePath -> IO ()
profile fp = forceReadPgn fp >>= putStr
