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

fileA = FileA <$ char 'a'
fileB = FileB <$ char 'b'
fileC = FileC <$ char 'c'
fileD = FileD <$ char 'd'
fileE = FileE <$ char 'e'
fileF = FileF <$ char 'f'
fileG = FileG <$ char 'g'
fileH = FileH <$ char 'h'
file = fileA <|> fileB <|> fileC <|> fileD <|> fileE <|> fileF <|> fileG <|> fileH

rank1 = Rank1 <$ char '1'
rank2 = Rank2 <$ char '2'
rank3 = Rank3 <$ char '3'
rank4 = Rank4 <$ char '4'
rank5 = Rank5 <$ char '5'
rank6 = Rank6 <$ char '6'
rank7 = Rank7 <$ char '7'
rank8 = Rank8 <$ char '8'
rank = rank1 <|> rank2 <|> rank3 <|> rank4 <|> rank5 <|> rank6 <|> rank7 <|> rank8

cell = createCell <$> file <*> rank
