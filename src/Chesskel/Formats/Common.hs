{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

{-|
Module      : Chesskel.Formats.Common
Description : Parsec parsing functions for cells and SAN moves.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions for parsing individual files, ranks, cells and
SAN moves. These functions are reused for multiple formats.
-}
module Chesskel.Formats.Common (
    file,
    rank,
    cell,
    sanMove
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Movement.Minimal
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Maybe
import Text.Parsec

-- |Parses a file (a-h).
file = cr <$> oneOf "abcdefgh" <?> "file (a-h)" where
    cr 'a' = FileA
    cr 'b' = FileB 
    cr 'c' = FileC
    cr 'd' = FileD
    cr 'e' = FileE
    cr 'f' = FileF
    cr 'g' = FileG
    cr 'h' = FileH
    cr _ = error "Should never happen. Call the police."

-- |Parses a rank (1-8).
rank = cr <$> oneOf "12345678" <?> "rank (1-8)" where
    cr '1' = Rank1
    cr '2' = Rank2 
    cr '3' = Rank3
    cr '4' = Rank4
    cr '5' = Rank5
    cr '6' = Rank6
    cr '7' = Rank7
    cr '8' = Rank8
    cr _ = error "Should never happen. Call the police."

-- |Parses a cell (file followed by rank).
cell = createCell <$> file <*> rank <?> "square (a1-h8)"

king = King <$ char 'K'
queen = Queen <$ char 'Q'
rook = Rook <$ char 'R'
bishop = Bishop <$ char 'B'
knight = Knight <$ char 'N'
pawn = Pawn <$ char 'P' -- Rarely used notation, but legal.
chessman = king <|> queen <|> rook <|> bishop <|> knight <|> pawn <?>
    "chessman (K=King, Q=Queen, R=Rook, B=Bishop, N=Knight or P=Pawn)"

check = Check <$ char '+'
checkmate = Checkmate <$ char '#'
checkState = check <|> checkmate <?>
    "check indicator (+ = check, # = checkmate)"

pQueen = PQueen <$ char 'Q'
pRook = PRook <$ char 'R'
pBishop = PBishop <$ char 'B'
pKnight = PKnight <$ char 'N'
pTarget = pQueen <|> pRook <|> pBishop <|> pKnight <?>
    "promotion target (Q=Queen, R=Rook, B=Bishop or N=Knight)"

pawnPromotion = char '=' *> pTarget

capture = void (char 'x')
pawnCapture = try (file <* capture) >>= pawnMoveBody . Just

pawnNonCapture = pawnMoveBody Nothing

pawnMoveBody mFromFile = do
    toCell <- cell
    mPromotionTarget <- optionMaybe pawnPromotion
    mCheckState <- optionMaybe checkState
    return MkUnderspecifiedMove {
        -- For pawns we have the convenient invariant that the move is a
        -- capture iff there's a file disambiguation.
        -- Rank disambiguations are also never applicable for pawns.
        knownChessman = Pawn,
        knownToCell = toCell,
        knownIsCapture = isJust mFromFile,
        knownFromFile = mFromFile,
        knownFromRank = Nothing,
        knownPromotionTarget = mPromotionTarget,
        knownCheckState = mCheckState
    }

pawnMove = pawnCapture <|> pawnNonCapture

bodyEnd checkCapture mFromFile mFromRank = do
    -- If checkCapture is false we always set mCapture to Nothing.
    mCapture <- if checkCapture then Just <$> capture else return Nothing
    toCell <- cell
    return (mFromFile, mFromRank, isJust mCapture, toCell)

noDisambiguation checkCapture = bodyEnd checkCapture Nothing Nothing

fileDisambiguation checkCapture = do
    fromFile <- file
    bodyEnd checkCapture (Just fromFile) Nothing

rankDisambiguation checkCapture = do
    fromRank <- rank
    bodyEnd checkCapture Nothing (Just fromRank)

fileAndRankDisambiguation checkCapture = do
    fromFile <- file
    fromRank <- rank
    bodyEnd checkCapture (Just fromFile) (Just fromRank)

justCapture = noDisambiguation True
justCaptureWithFile = fileDisambiguation True
justCaptureWithRank = rankDisambiguation True
justCaptureWithFileAndRank = fileAndRankDisambiguation True

nonCapture = noDisambiguation False
nonCaptureWithFile = fileDisambiguation False
nonCaptureWithRank = rankDisambiguation False
nonCaptureWithFileAndRank = fileAndRankDisambiguation False

nonPawnCaptureBody = try justCaptureWithFileAndRank <|>
                     try justCaptureWithFile <|>
                     try justCaptureWithRank <|>
                     try justCapture

nonPawnNonCaptureBody = try nonCaptureWithFileAndRank <|>
                        try nonCaptureWithFile <|>
                        try nonCaptureWithRank <|>
                        try nonCapture

nonPawnMoveBody = nonPawnCaptureBody <|> nonPawnNonCaptureBody

nonPawnMove = do
    cm <- chessman
    (mFromFile, mFromRank, moveIsCapture, toCell) <- nonPawnMoveBody
    mCheckState <- optionMaybe checkState
    return MkUnderspecifiedMove {
        knownChessman = cm,
        knownToCell = toCell,
        knownIsCapture = moveIsCapture,
        knownFromFile = mFromFile,
        knownFromRank = mFromRank,
        knownPromotionTarget = Nothing,
        knownCheckState = mCheckState
    }

castleShort = CastleMove Kingside <$ (string "O-O" <|> string "0-0")
castleLong = CastleMove Queenside <$ (string "O-O-O" <|> string "0-0-0")
castling = try castleLong <|> castleShort <?>
    "castling (O-O or O-O-O)"

-- |A parser for a single Standard Algebraic Notation move.
sanMove = castling <|> pawnMove <|> nonPawnMove
