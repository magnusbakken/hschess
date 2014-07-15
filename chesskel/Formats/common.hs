{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.Common (
    file,
    rank,
    cell
) where

import Chesskel.Board 
import Control.Applicative hiding ((<|>), many)
import Text.Parsec

file = cr <$> oneOf "abcdefgh" <?> "file (a-h)" where
    cr 'a' = FileA
    cr 'b' = FileB 
    cr 'c' = FileC
    cr 'd' = FileD
    cr 'e' = FileE
    cr 'f' = FileF
    cr 'g' = FileG
    cr 'h' = FileH

rank = cr <$> oneOf "12345678" <?> "rank (1-8)" where
    cr '1' = Rank1
    cr '2' = Rank2 
    cr '3' = Rank3
    cr '4' = Rank4
    cr '5' = Rank5
    cr '6' = Rank6
    cr '7' = Rank7
    cr '8' = Rank8

cell = createCell <$> file <*> rank <?> "square (a1-h8)"
