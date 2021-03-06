{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

{-|
Module      : Chesskel.Formats.San
Description : Utilities for reading and writing SAN strings.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions for reading and writing chess moves in the SAN
(Standard Algebraic Notation) format.
-}
module Chesskel.Formats.San (
    SanMoveError (..),
    readSanMove,
    writeSanMove
) where

import Chesskel.Formats.Common
import Chesskel.Movement.Minimal
import Control.Applicative
import Text.Parsec

-- | An error indicating that there's something wrong with a SAN move.
data SanMoveError =
    -- |The SAN move has a syntax error. Currently this is the only type of
    --  error for SAN moves.
    SanMoveSyntaxError String deriving (Eq, Show)

mapSanMoveError :: Either ParseError a -> Either SanMoveError a
mapSanMoveError (Right a) = Right a
mapSanMoveError (Left e) = Left $ SanMoveSyntaxError errMsg where
    errMsg = "Invalid syntax: " ++ show e

san = sanMove <* eof

-- |Reads a single SAN move, or returns a SanMoveError if the move is
--  syntactically invalid.
readSanMove :: String -> Either SanMoveError UnderspecifiedMove
readSanMove = mapSanMoveError . parse san "ReadSanMove"

-- |Writes a string for a single SAN move based on an UnderspecifiedMove.
--
--  Currently this function is completely equivalent to calling 'GHC.show' on
--  the underspecified move. The function is included so this SAN module has
--  the same structure as the other format modules.
writeSanMove :: UnderspecifiedMove -> String
writeSanMove = show
