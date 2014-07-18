{-|
Module      : Chesskel
Description : External chess format API.
Copyright   : © Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module combines the modules for all supported chess interchange formats.
Currently this includes the PGN (Portable Game Notation) full game format, the
FEN (Forsyth-Edwards Notation) single position format, and the SAN (Standard
Algebraic Notation) move format.
-}
module Chesskel.Formats (
    -- |The "Chesskel.Formats.Fen" module contains functions for reading and writing FEN (Forsyth–Edwards Notation) strings.
    module Chesskel.Formats.Fen,
    
    -- |The "Chesskel.Formats.Pgn" module contains functions for reading and writing PGN (Portable Game Notation) strings.
    module Chesskel.Formats.Pgn,
    
    -- |The "Chesskel.Formats.San" module contains functions for reading and writing SAN (Standard Algebraic Notation) strings.
    module Chesskel.Formats.San
) where

import Chesskel.Formats.Fen
import Chesskel.Formats.Pgn
import Chesskel.Formats.San
