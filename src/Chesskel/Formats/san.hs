{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.San (
    SanMoveError (..),
    readSanMove,
    writeSanMove
) where

import Chesskel.Formats.Common
import Chesskel.Movement
import Text.Parsec

-- | An error indicating that there's something wrong with a SAN move.
data SanMoveError =
    -- |The SAN move has a syntax error. Currently this is the only type of error for SAN moves.
    SanMoveSyntaxError String deriving (Eq)

mapSanMoveError :: Either ParseError a -> Either SanMoveError a
mapSanMoveError (Right a) = Right a
mapSanMoveError (Left e) = Left $ SanMoveSyntaxError $ "Invalid syntax: " ++ show e

-- |Reads a single SAN move, or returns a SanMoveError if the move is syntactically invalid.
readSanMove :: String -> Either SanMoveError UnderspecifiedMove
readSanMove = mapSanMoveError . parse sanMove "ReadSanMove"

-- |Writes a string for a single SAN move based on an UnderspecifiedMove.
--
--  Currently this function is completely equivalent to calling 'GHC.show' on the underspecified move.
--  The function is included so this SAN module has the same structure as the other format modules.
writeSanMove :: UnderspecifiedMove -> String
writeSanMove = show
