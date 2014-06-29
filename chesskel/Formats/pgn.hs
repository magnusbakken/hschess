module Chesskel.Formats.Pgn (
    parsePgn
) where

import Chesskel.Gameplay

data PgnParseError =
    HeaderSyntaxError |
    MovetextSyntaxError |
    ContainsIllegalMove deriving (Eq, Show)

parsePgn :: String -> Either PgnParseError GameContext
parsePgn = undefined