{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

{-|
Module      : Chesskel.Formats.Fen
Description : Utilities for reading and writing FEN strings.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions for reading and writing chess positions in the FEN
(Forsyth-Edwards Notation) format.
-}
module Chesskel.Formats.Fen (
    FenError (..),
    readFen,
    writeFen
) where

import Chesskel.Board
import Chesskel.Formats.Common
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Traversable
import Text.Parsec
import Text.Parsec.Token hiding (squares)
import Text.Parsec.Language (haskell)

-- |An error indicating that there's something wrong with a FEN.
data FenError =
    -- |The FEN has a syntax error.
    FenSyntaxError String |
    
    -- |The FEN is syntactically valid, but has an incorrect board description
    --  where at least one row has too many or too few cells.
    FenPositionError String deriving (Eq, Show)

data FenToken = Fen {
    piecePlacementToken :: PiecePlacementToken,
    currentPlayerToken :: CurrentPlayerToken,
    castlingRightsToken :: CastlingRightsToken,
    previousEnPassantCellToken :: PreviousEnPassantCellToken,
    halfMoveClockToken :: HalfMoveClockToken,
    fullMoveNumberToken :: FullMoveNumberToken
} deriving (Eq)

data PiecePlacementToken = PiecePlacement {
    row8 :: RowToken,
    row7 :: RowToken,
    row6 :: RowToken,
    row5 :: RowToken,
    row4 :: RowToken,
    row3 :: RowToken,
    row2 :: RowToken,
    row1 :: RowToken
} deriving (Eq)

type RowToken = [SquareToken] -- TODO: Maybe use a GADT or something for maximum type safety here?
data SquareToken = FenPiece Piece | EmptySpaceCount Int deriving (Eq)
newtype CurrentPlayerToken = CurrentPlayer Color deriving (Eq)
newtype CastlingRightsToken = CastlingRights (S.Set Castling) deriving (Eq)
newtype PreviousEnPassantCellToken = PreviousEnPassantCell (Maybe Cell) deriving (Eq)
newtype HalfMoveClockToken = HalfMoveClock Int deriving (Eq)
newtype FullMoveNumberToken = FullMoveNumber Int deriving (Eq)

fp chessman color c = (chessman, color) <$ char c
pawn = fp Pawn White 'P' <|> fp Pawn Black 'p'
knight = fp Knight White 'N' <|> fp Knight Black 'n'
bishop = fp Bishop White 'B' <|> fp Bishop Black 'b'
rook = fp Rook White 'R' <|> fp Rook Black 'r'
queen = fp Queen White 'Q' <|> fp Queen Black 'q'
king = fp King White 'K' <|> fp King Black 'k'
fenPiece = FenPiece <$> (pawn <|> knight <|> bishop <|> rook <|> queen <|> king)

emptySpaceCount = EmptySpaceCount . digitToInt <$> oneOf "12345678"

square = fenPiece <|> emptySpaceCount

rowSeparator = () <$ char '/'
squares = many square
piecePlacement = do
    r8 <- squares
    rowSeparator
    r7 <- squares
    rowSeparator
    r6 <- squares
    rowSeparator
    r5 <- squares
    rowSeparator
    r4 <- squares
    rowSeparator
    r3 <- squares
    rowSeparator
    r2 <- squares
    rowSeparator
    r1 <- squares
    return PiecePlacement {
        row8 = r8,
        row7 = r7,
        row6 = r6,
        row5 = r5,
        row4 = r4,
        row3 = r3,
        row2 = r2,
        row1 = r1
    }

white = CurrentPlayer White <$ char 'w'
black = CurrentPlayer Black <$ char 'b'
pCurrentPlayer = white <|> black

kingsideWhite = 'K'
queensideWhite = 'Q'
kingsideBlack = 'k'
queensideBlack = 'q'

noCastlingRights = CastlingRights S.empty <$ char '-'
hasCastlingRights c direction color = do
    m <- optionMaybe (() <$ char c)
    return (if isJust m then Just (direction, color) else Nothing)
withCastlingRights = do
    mWhiteKingside <- hasCastlingRights kingsideWhite Kingside White
    mWhiteQueenside <- hasCastlingRights queensideWhite Queenside White
    mBlackKingside <- hasCastlingRights kingsideBlack Kingside Black
    mBlackQueenside <- hasCastlingRights queensideBlack Queenside Black
    let castling = catMaybes [mWhiteKingside, mWhiteQueenside, mBlackKingside, mBlackQueenside]
    if null castling
        then parserFail "At least one castling rights token must be present, or '-' should be used."
        else return $ CastlingRights $ S.fromList castling

pCastlingRights = noCastlingRights <|> withCastlingRights

noEnPassant = PreviousEnPassantCell Nothing <$ char '-'
withEnPassant = PreviousEnPassantCell . Just <$> cell
pPreviousEnPassantCell = withEnPassant <|> noEnPassant

pHalfMoveClock = HalfMoveClock . fromEnum <$> natural haskell

fullMoveNumber = FullMoveNumber . fromEnum <$> natural haskell

fen = do
    rows <- piecePlacement
    char ' '
    color <- pCurrentPlayer
    char ' '
    castling <- pCastlingRights
    char ' '
    enPassant <- pPreviousEnPassantCell
    char ' '
    -- These last two parsers parse the whitespace on their own.
    halfMoves <- pHalfMoveClock
    fullMoves <- fullMoveNumber
    eof
    return Fen {
        piecePlacementToken = rows,
        currentPlayerToken = color,
        castlingRightsToken = castling,
        previousEnPassantCellToken = enPassant,
        halfMoveClockToken = halfMoves,
        fullMoveNumberToken = fullMoves
    }

expandRow :: RowToken -> [Square]
expandRow [] = []
expandRow (EmptySpaceCount 0 : xs) = expandRow xs
expandRow (EmptySpaceCount n : xs) = Nothing : expandRow (EmptySpaceCount (n-1) : xs)
expandRow (FenPiece piece : xs) = Just piece : expandRow xs

interpretRow :: RowToken -> Either FenError [Square]
interpretRow rowToken = do
    let row = expandRow rowToken
    if length row == 8
        then Right row
        else Left $ FenPositionError "Incorrect number of cells in row. There should be eight cells."

interpretRows :: [RowToken] -> Either FenError [[Square]]
interpretRows rows = for rows interpretRow

interpretPiecePlacement :: PiecePlacementToken -> Either FenError Position
interpretPiecePlacement ppt = do
    rows <- interpretRows [row1 ppt, row2 ppt, row3 ppt, row4 ppt, row5 ppt, row6 ppt, row7 ppt, row8 ppt]
    return $ createPosition (concat rows)

interpretFen :: FenToken -> Either FenError PositionContext
interpretFen fenToken = do
    pos <- interpretPiecePlacement (piecePlacementToken fenToken)
    let (CurrentPlayer color) = currentPlayerToken fenToken
        (CastlingRights castling) = castlingRightsToken fenToken
        (PreviousEnPassantCell enPassant) = previousEnPassantCellToken fenToken
        (HalfMoveClock halfMoves) = halfMoveClockToken fenToken
        (FullMoveNumber fullMoves) = fullMoveNumberToken fenToken
    return PC {
        position = pos,
        currentPlayer = color,
        castlingRights = castling,
        previousEnPassantCell = enPassant,
        halfMoveClock = halfMoves,
        moveCount = fullMoves
    }

mapSyntaxError :: Either ParseError a -> Either FenError a
mapSyntaxError (Left e) = Left $ FenSyntaxError (show e)
mapSyntaxError (Right a) = Right a

collapseRow :: [Square] -> RowToken
collapseRow = ungroup . L.group where
    ungroup [] = []
    ungroup ((Nothing:xs) : xss) = (EmptySpaceCount $ length xs + 1) : ungroup xss
    ungroup (xs : xss) = map FenPiece (catMaybes xs) ++ ungroup xss

createPiecePlacement :: Position -> PiecePlacementToken
createPiecePlacement pos =
    let [r1, r2, r3, r4, r5, r6, r7, r8] = map collapseRow (getRows pos) in
    PiecePlacement r8 r7 r6 r5 r4 r3 r2 r1

createSyntaxTree :: PositionContext -> FenToken
createSyntaxTree pc = Fen {
    piecePlacementToken = createPiecePlacement (position pc),
    currentPlayerToken = CurrentPlayer (currentPlayer pc),
    castlingRightsToken = CastlingRights (castlingRights pc),
    previousEnPassantCellToken = PreviousEnPassantCell (previousEnPassantCell pc),
    halfMoveClockToken = HalfMoveClock (halfMoveClock pc),
    fullMoveNumberToken = FullMoveNumber (moveCount pc)
}

showRow :: RowToken -> String
showRow = map squareToChar where
    squareToChar (EmptySpaceCount n) = intToDigit n
    squareToChar (FenPiece piece) = pieceToChar piece

showPiecePlacement :: PiecePlacementToken -> String
showPiecePlacement ppt =
    let rows = [row8 ppt, row7 ppt, row6 ppt, row5 ppt, row4 ppt, row3 ppt, row2 ppt, row1 ppt] in
    L.intercalate "/" (map showRow rows)

showCurrentPlayer :: CurrentPlayerToken -> String
showCurrentPlayer (CurrentPlayer White) = "w"
showCurrentPlayer (CurrentPlayer Black) = "b"

showCastlingRights :: CastlingRightsToken -> String
showCastlingRights (CastlingRights cr)
    | S.null cr = "-"
    | otherwise = catMaybes [sKingsideWhite, sQueensideWhite, sKingsideBlack, sQueensideBlack] where
        has side color = (side, color) `S.member` cr
        sKingsideWhite = if has Kingside White then Just kingsideWhite else Nothing
        sQueensideWhite = if has Queenside White then Just queensideWhite else Nothing
        sKingsideBlack = if has Kingside Black then Just kingsideBlack else Nothing
        sQueensideBlack = if has Queenside Black then Just queensideBlack else Nothing

showPreviousEnPassantCell :: PreviousEnPassantCellToken -> String
showPreviousEnPassantCell (PreviousEnPassantCell Nothing) = "-"
showPreviousEnPassantCell (PreviousEnPassantCell (Just c)) = show c

showHalfMoveClock :: HalfMoveClockToken -> String
showHalfMoveClock (HalfMoveClock n) = show n

showFullMoveNumber :: FullMoveNumberToken -> String
showFullMoveNumber (FullMoveNumber n) = show n

showSyntaxTree :: FenToken -> String
showSyntaxTree fenToken = unwords [
    showPiecePlacement (piecePlacementToken fenToken),
    showCurrentPlayer (currentPlayerToken fenToken),
    showCastlingRights (castlingRightsToken fenToken),
    showPreviousEnPassantCell (previousEnPassantCellToken fenToken),
    showHalfMoveClock (halfMoveClockToken fenToken),
    showFullMoveNumber (fullMoveNumberToken fenToken)]

-- |Parses a FEN string and creates a PositionContext,
--  or returns a FenError if the FEN is syntactically or semantically invalid.
readFen :: String -> Either FenError PositionContext
readFen s = mapSyntaxError (parse fen "ParseFen" s) >>= interpretFen

-- |Writes a FEN string based on a PositionContext.
--  Unlike 'readFen', this function should never fail.
writeFen :: PositionContext -> String
writeFen = showSyntaxTree . createSyntaxTree
