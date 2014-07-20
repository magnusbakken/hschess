{-|
Module      : Chesskel.Board
Description : Functions for chess positions, pieces and cells.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions related to chess positions, pieces and cells.
Basically everything that's \"static\", with no implication of movement,
is placed in this model. (The 'Castling' type could be seen as an exception to
this. It's kept in this module because of the need for keeping track of
castling rights.)
-}
module Chesskel.Board (
    Position,
    PositionContext (..),
    Chessman (..),
    Color (..),
    Piece,
    Square,
    Rank (..),
    File (..),
    Cell (..),
    Castling,
    CastlingDirection (..),
    
    -- ** Position functions
    -- |Functions that create and manipulate position objects.
    
    startPosition,
    emptyPosition,
    standardPosition,
    createPosition,
    updatePosition,
    
    -- ** Assorted functions
    -- |Functions that manipulate cells, colors and castling specifications.
    
    otherColor,
    createCell,
    allCells,
    allCastlingRights,
    
    -- ** Piece and square functions
    -- |Functions that retrieve information about pieces and squares.
    
    hasPiece,
    hasPieceOfType,
    hasPieceOfColor,
    piecesOfColor,
    getSquare,
    getRows,
    pieceToChar,
    
    -- ** Cell literals
    -- |These cell literals are provided for testing and debugging purposes.
    --  It's quite useful to use these to test ad-hoc moves and games:
    --
    --  > playNonPromotionMove (createMove e2 e4) startStandardGame
    --
    --  However, if they turn out to be too much a nuisance on account of
    --  having simple names that may clash with many other variables and
    --  functions, they may be moved to a separate module.
    
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    
    -- ** Piece functions
    -- |Like the cell literals, these piece functions are provided mainly for
    --  testing and debugging purposes.
    
    whiteKing, whiteQueen, whiteRook, whiteBishop, whiteKnight, whitePawn,
    blackKing, blackQueen, blackRook, blackBishop, blackKnight, blackPawn
) where

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

-- |A chessman is one of the six basic piece types of chess.
data Chessman = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Bounded, Show)

-- |Each color represents one of the two players.
--
--  Note! It may seem surprising that Color is part of the 'Ord' class, with
--  'White' < 'Black'. The reason is that the IntMap data structure is
--  currently used internally, and it requires the data you put in it to be
--  ordered. The Ord instance for Color should not be relied upon, and may be
--  removed in the future.
data Color = White | Black deriving (Eq, Ord, Enum, Bounded, Show)

-- |A piece is a combination of a chessman and a color.
type Piece = (Chessman, Color)

-- |A square is either a piece or Nothing.
--
--  Squares don't know where they are on the board. To identify board
--  coordinates you need to use the Cell type.
type Square = Maybe Piece

-- |A vector of squares, currently used as the internal storage mechanism.
type Squares = V.Vector Square

-- |A piece map, used for caching of piece locations.
type PieceMap = IM.IntMap Piece

-- |A cache data structure used for the position type.
--  This is an implementation detail, and may be changed or removed in the
--  future.
newtype PositionCache = PositionCache PieceMap deriving (Eq)

-- |A position represents a single setup of pieces on the chess board.
--
--  This type is not sufficient for determining the current position of a game,
--  since it contains only the board and doesn't know anything about the
--  current player, castling rights, etc. The type that has all the remaining
--  position information is the 'PositionContext' type.
--
--  This data type is abstract. Consumers should not have to care what storage
--  mechanism is used internally.
newtype Position = Position (Squares, PositionCache) deriving (Eq)

-- |A position context is a wrapper for the 'Position' type that contains extra
--  information that cannot be extrapolated from the board itself.
--
--  This data structure contains all the necessary information to create a FEN.
data PositionContext = MkPositionContext {
    -- |The actual position.
    position :: Position,
    
    -- |The player whose turn it is to move.
    currentPlayer :: Color,
    
    -- |A set of castling rights for both players.
    --
    --  Players lose all castling rights when they move their king, and lose
    --  castling rights on one side when they move the rook on that side.
    castlingRights :: S.Set Castling,
    
    -- |The previous en passant cell.
    --
    --  When the previous move was a double pawn move, this will be the cell
    --  that was skipped. For instance, if the previous move was white moving a
    --  pawn from d2 to d4, this will be set to d3.
    previousEnPassantCell :: Maybe Cell,
    
    -- |The number of half-moves (plies) since a pawn was moved or a piece was
    --  captured.
    --
    --  This number is used for the 50 move rule, which lets either player
    --  claim a draw when this number reaches 50.
    halfMoveClock :: Int,
    
    -- |The number of full moves (one move by each player) since the beginning
    --  of the game.
    moveCount :: Int
} deriving (Eq)

-- |The chess board is made up of eight ranks, numbered 1 through 8.
data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
    deriving (Eq, Ord, Bounded, Show)

-- |The chess board is made up of eight files, labeled a through h.
data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
    deriving (Eq, Ord, Bounded, Show)

-- |A cell represents the coordinates of one of the tiles on the chess board.
--
--  Cells are combinations of files and ranks, and is usually designated by
--  names such as a1, a2, etc.
newtype Cell = Cell (File, Rank) deriving (Eq, Ord, Bounded)

-- |The directions in which the king can castle.
data CastlingDirection = Kingside | Queenside deriving (Eq, Ord)

-- |A full specification of a castling event requires the castling direction
--  and the player.
type Castling = (CastlingDirection, Color)

instance Enum Rank where
    fromEnum Rank1 = 1
    fromEnum Rank2 = 2
    fromEnum Rank3 = 3
    fromEnum Rank4 = 4
    fromEnum Rank5 = 5
    fromEnum Rank6 = 6
    fromEnum Rank7 = 7
    fromEnum Rank8 = 8
    toEnum 1 = Rank1
    toEnum 2 = Rank2
    toEnum 3 = Rank3
    toEnum 4 = Rank4
    toEnum 5 = Rank5
    toEnum 6 = Rank6
    toEnum 7 = Rank7
    toEnum 8 = Rank8
    toEnum n = error $ "tag " ++ show n ++ " is outside of bounds (1, 8)"

instance Enum File where
    fromEnum FileA = 1
    fromEnum FileB = 2
    fromEnum FileC = 3
    fromEnum FileD = 4
    fromEnum FileE = 5
    fromEnum FileF = 6
    fromEnum FileG = 7
    fromEnum FileH = 8
    toEnum 1 = FileA
    toEnum 2 = FileB
    toEnum 3 = FileC
    toEnum 4 = FileD
    toEnum 5 = FileE
    toEnum 6 = FileF
    toEnum 7 = FileG
    toEnum 8 = FileH
    toEnum n = error $ "tag " ++ show n ++ " is outside of bounds (1, 8)"

instance Enum Cell where
    fromEnum (Cell (file, rank)) = 8 * (fromEnum rank - 1) + fromEnum file - 1
    toEnum n
        | n >= 0 && n < 64 = Cell (toEnum (r+1), toEnum (q+1))
        | otherwise = error errMsg where
            (q, r) = n `quotRem` 8
            errMsg = "tag " ++ show n ++ " is outside of bounds (0, 63)"

instance Show Cell where
    show (Cell (file, rank)) = [shortFile file, shortRank rank]

instance Show CastlingDirection where
    show Kingside = "O-O"
    show Queenside = "O-O-O"

instance Show Position where
    show = unlines . map showRow . reverse . getRows

instance Show PositionContext where
    show pc = shows (position pc) ("\n" ++ showPlayerToMove pc) where
        showPlayerToMove MkPositionContext { currentPlayer = White } = "White to move"
        showPlayerToMove MkPositionContext { currentPlayer = Black } = "Black to move"

-- |Gets the letter corresponding to the given file.
shortFile :: File -> Char
shortFile FileA = 'a'
shortFile FileB = 'b'
shortFile FileC = 'c'
shortFile FileD = 'd'
shortFile FileE = 'e'
shortFile FileF = 'f'
shortFile FileG = 'g'
shortFile FileH = 'h'

-- |Gets the number corresponding to the given rank.
shortRank :: Rank -> Char
shortRank Rank1 = '1'
shortRank Rank2 = '2'
shortRank Rank3 = '3'
shortRank Rank4 = '4'
shortRank Rank5 = '5'
shortRank Rank6 = '6'
shortRank Rank7 = '7'
shortRank Rank8 = '8'

-- |Gets the letter corresponding to the given piece.
--
--  This is used for the Show instance of the Position type.
pieceToChar :: Piece -> Char
pieceToChar (Pawn, color) = colorize 'P' color
pieceToChar (Knight, color) = colorize 'N' color
pieceToChar (Bishop, color) = colorize 'B' color
pieceToChar (Rook, color) = colorize 'R' color
pieceToChar (Queen, color) = colorize 'Q' color
pieceToChar (King, color) = colorize 'K' color

-- |Make the given character lowercase if the given color is black.
colorize :: Char -> Color -> Char
colorize c color = if color == Black then toLower c else c

-- |Shows a single square.
showSquare :: Square -> String
showSquare Nothing = " "
showSquare (Just piece) = [pieceToChar piece]

-- |Shows a row of squares.
showRow :: [Square] -> String
showRow = unwords . map showSquare

-- | The opposite color of the given one.
-- 
-- >  otherColor White == Black
-- >  otherColor Black == White
otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

-- |Determines whether the given piece has the given color.
isColor :: Color -> Piece -> Bool
isColor color (_, pieceColor) = color == pieceColor

-- |A set containing all castling specifications.
allCastlingRights :: S.Set Castling
allCastlingRights = S.fromList [
    (Kingside, White),
    (Kingside, Black),
    (Queenside, White),
    (Queenside, Black)]

-- |Gets a list of lists, where each list is a row from the standard starting
--  position of chess.
startRows :: [[Square]]
startRows = [
    backRow White,
    pawnRow White,
    emptyRow,
    emptyRow,
    emptyRow,
    emptyRow,
    pawnRow Black,
    backRow Black]

-- |Gets the back row setup for the standard starting position of chess.
backRow :: Color -> [Square]
backRow color =
    map getPiece [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] where
        getPiece chessman = Just (chessman, color)

-- |Gets a row full of pawns.
pawnRow :: Color -> [Square]
pawnRow color = replicate 8 $ Just (Pawn, color)

-- |Gets an empty row.
emptyRow :: [Square]
emptyRow = replicate 8 Nothing

-- |Gets a list of rows (list of squares) for the given position.
getRows :: Position -> [[Square]]
getRows (Position (vector, _)) =
    map (\n -> V.toList $ V.slice (n*8) 8 vector) [0..7]

-- |Creates a cell based on a file and rank.
createCell :: File -> Rank -> Cell
createCell = curry Cell

-- |Gets a sorted list containing all cells.
allCells :: [Cell]
allCells = enumFromTo minBound maxBound

-- |Gets the square at the given cell in a given position.
getSquare :: Position -> Cell -> Square
getSquare (Position (vector, _)) cell = vector V.! fromEnum cell

-- |Determines whether there's a piece at the given cell in the given position.
hasPiece :: Position -> Cell -> Bool
hasPiece (Position (_, PositionCache pieceMap)) cell =
    fromEnum cell `IM.member` pieceMap

-- |Determines whether there's a piece of some type at some cell in a position.
hasPieceOfType :: Piece -> Position -> Cell -> Bool
hasPieceOfType piece = hasPieceBy (== piece)

-- |Determines whether there's a piece of some color at some cell in a position.
hasPieceOfColor :: Color -> Position -> Cell -> Bool
hasPieceOfColor color = hasPieceBy (isColor color)

-- |Applies a custom predicate to the piece at a given cell in the position.
hasPieceBy :: (Piece -> Bool) -> Position -> Cell -> Bool
hasPieceBy predicate pos cell = maybe False predicate (getSquare pos cell)

-- |Gets a list of pieces and their integer indices in the cached IntMap from
--  the given squares.
piecesOnly :: [Square] -> [(Int, Piece)]
piecesOnly squares = do
    (idx, square) <- zip [0..] squares
    guard $ isJust square
    return (idx, fromJust square)

-- |Makes a new piece IntMap based on the pieces in the given list of squares.
makePieceMap :: [Square] -> PieceMap
makePieceMap = IM.fromList . piecesOnly

-- |Gets a list of all pieces and their positions from the piece IntMap.
getPiecesFromMap :: PieceMap -> [(Cell, Piece)]
getPiecesFromMap = map (first toEnum) . IM.assocs

-- |Gets all pieces of the given color in the given position, as well as their
--  locations in the position.
piecesOfColor :: Color -> Position -> [(Cell, Piece)]
piecesOfColor color (Position (_, PositionCache pieceMap)) =
    filter (isColor color . snd) $ getPiecesFromMap pieceMap

-- |The standard starting position of chess.
startPosition :: PositionContext
startPosition = MkPositionContext {
    position = standardPosition,
    currentPlayer = White,
    castlingRights = allCastlingRights,
    previousEnPassantCell = Nothing,
    halfMoveClock = 0,
    moveCount = 1
}

-- |Gets an empty board.
emptyPosition :: Position
emptyPosition = createPosition $ repeat Nothing

-- |Gets a board with the standard starting position of chess.
standardPosition :: Position
standardPosition = createPosition $ concat startRows

-- |Creates a new position based on the given list of squares.
--
--  Behavior is undefined if the list doesn't have precisely 64 items in it.
createPosition :: [Square] -> Position
createPosition squares =
    Position (V.fromListN 64 squares, createPositionCache squares)

-- |Creates a position cache based on the given list of squares.
createPositionCache :: [Square] -> PositionCache
createPositionCache = PositionCache . makePieceMap

-- |Updates the given position with a list of changes.
--
--  Each update indicates what cell to change, and a Square object which
--  indicates whether to put a piece there, or to vacate the cell if the value
--  is 'Nothing'.
updatePosition :: Position -> [(Cell, Square)] -> Position
updatePosition (Position (board, cache)) updates =
    Position (updateBoard board updates, updatePositionCache cache updates)

-- |Updates all cells in the square vector. This is an O(n+m) operation.
updateBoard :: V.Vector Square -> [(Cell, Square)] -> V.Vector Square
updateBoard vector updates = vector V.// map (first fromEnum) updates

-- |Updates the position cache with the given updates.
updatePositionCache :: PositionCache -> [(Cell, Square)] -> PositionCache
updatePositionCache = foldl alterPositionCache where
    alterPositionCache (PositionCache pieceMap) (cell, square) =
        PositionCache $ IM.alter (const square) (fromEnum cell) pieceMap

a1, b1, c1, d1, e1, f1, g1, h1 :: Cell
a2, b2, c2, d2, e2, f2, g2, h2 :: Cell
a3, b3, c3, d3, e3, f3, g3, h3 :: Cell
a4, b4, c4, d4, e4, f4, g4, h4 :: Cell
a5, b5, c5, d5, e5, f5, g5, h5 :: Cell
a6, b6, c6, d6, e6, f6, g6, h6 :: Cell
a7, b7, c7, d7, e7, f7, g7, h7 :: Cell
a8, b8, c8, d8, e8, f8, g8, h8 :: Cell
a1 = Cell (FileA, Rank1)
b1 = Cell (FileB, Rank1)
c1 = Cell (FileC, Rank1)
d1 = Cell (FileD, Rank1)
e1 = Cell (FileE, Rank1)
f1 = Cell (FileF, Rank1)
g1 = Cell (FileG, Rank1)
h1 = Cell (FileH, Rank1)
a2 = Cell (FileA, Rank2)
b2 = Cell (FileB, Rank2)
c2 = Cell (FileC, Rank2)
d2 = Cell (FileD, Rank2)
e2 = Cell (FileE, Rank2)
f2 = Cell (FileF, Rank2)
g2 = Cell (FileG, Rank2)
h2 = Cell (FileH, Rank2)
a3 = Cell (FileA, Rank3)
b3 = Cell (FileB, Rank3)
c3 = Cell (FileC, Rank3)
d3 = Cell (FileD, Rank3)
e3 = Cell (FileE, Rank3)
f3 = Cell (FileF, Rank3)
g3 = Cell (FileG, Rank3)
h3 = Cell (FileH, Rank3)
a4 = Cell (FileA, Rank4)
b4 = Cell (FileB, Rank4)
c4 = Cell (FileC, Rank4)
d4 = Cell (FileD, Rank4)
e4 = Cell (FileE, Rank4)
f4 = Cell (FileF, Rank4)
g4 = Cell (FileG, Rank4)
h4 = Cell (FileH, Rank4)
a5 = Cell (FileA, Rank5)
b5 = Cell (FileB, Rank5)
c5 = Cell (FileC, Rank5)
d5 = Cell (FileD, Rank5)
e5 = Cell (FileE, Rank5)
f5 = Cell (FileF, Rank5)
g5 = Cell (FileG, Rank5)
h5 = Cell (FileH, Rank5)
a6 = Cell (FileA, Rank6)
b6 = Cell (FileB, Rank6)
c6 = Cell (FileC, Rank6)
d6 = Cell (FileD, Rank6)
e6 = Cell (FileE, Rank6)
f6 = Cell (FileF, Rank6)
g6 = Cell (FileG, Rank6)
h6 = Cell (FileH, Rank6)
a7 = Cell (FileA, Rank7)
b7 = Cell (FileB, Rank7)
c7 = Cell (FileC, Rank7)
d7 = Cell (FileD, Rank7)
e7 = Cell (FileE, Rank7)
f7 = Cell (FileF, Rank7)
g7 = Cell (FileG, Rank7)
h7 = Cell (FileH, Rank7)
a8 = Cell (FileA, Rank8)
b8 = Cell (FileB, Rank8)
c8 = Cell (FileC, Rank8)
d8 = Cell (FileD, Rank8)
e8 = Cell (FileE, Rank8)
f8 = Cell (FileF, Rank8)
g8 = Cell (FileG, Rank8)
h8 = Cell (FileH, Rank8)

whiteKing, whiteQueen, whiteRook, whiteBishop, whiteKnight, whitePawn :: Piece
blackKing, blackQueen, blackRook, blackBishop, blackKnight, blackPawn :: Piece
whiteKing = (King, White)
whiteQueen = (Queen, White)
whiteRook = (Rook, White)
whiteBishop = (Bishop, White)
whiteKnight = (Knight, White)
whitePawn = (Pawn, White)
blackKing = (King, Black)
blackQueen = (Queen, Black)
blackRook = (Rook, Black)
blackBishop = (Bishop, Black)
blackKnight = (Knight, Black)
blackPawn = (Pawn, Black)
