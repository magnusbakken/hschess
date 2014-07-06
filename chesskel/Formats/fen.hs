{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.Fen (
    readFen
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Formats.Common
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.Maybe
import qualified Data.Set as S
import Data.Traversable
import Text.Parsec
import Text.Parsec.Token hiding (squares)
import Text.Parsec.Language (haskell)

data FenError =
    FenSyntaxError String |
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

noCastlingRights = CastlingRights S.empty <$ char '-'
hasCastlingRights c direction color = do
    m <- optionMaybe (() <$ char c)
    return (if isJust m then Just (direction, color) else Nothing)
withCastlingRights = do
    mWhiteKingside <- hasCastlingRights 'K' Kingside White
    mWhiteQueenside <- hasCastlingRights 'Q' Queenside White
    mBlackKingside <- hasCastlingRights 'k' Kingside Black
    mBlackQueenside <- hasCastlingRights 'q' Queenside Black
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

readFen :: String -> Either FenError PositionContext
readFen s = mapSyntaxError (parse fen "ParseFen" s) >>= interpretFen
