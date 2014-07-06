module Chesskel.Board (
    Piece,
    Chessman (..),
    Color (..),
    Square,
    Rank (..),
    File (..),
    Cell (..),
    Position,
    pieceToChar,
    otherColor,
    createCell,
    piecesOfColor,
    getRows,
    allCells,
    getSquare,
    hasPiece,
    hasPieceOfType,
    hasPieceOfColor,
    emptyPosition,
    standardPosition,
    createPosition,
    updatePosition,
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8
) where

import Control.Arrow
import Data.Char
import Data.Maybe
import qualified Data.Vector as V

data Chessman = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Color = White | Black deriving (Eq, Ord, Show)
type Piece = (Chessman, Color)
type Square = Maybe Piece
newtype Position = Position (V.Vector Square) deriving (Eq)

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8 deriving (Eq, Ord, Bounded, Show)
data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH deriving (Eq, Ord, Bounded, Show)
newtype Cell = Cell (File, Rank) deriving (Eq, Ord, Bounded)

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
        | n >= 0 && n < 64 = let (q, r) = n `quotRem` 8 in Cell (toEnum (r+1), toEnum (q+1))
        | otherwise = error $ "tag " ++ show n ++ " is outside of bounds (0, 63)"

instance Show Cell where
    show (Cell (file, rank)) = [shortFile file, shortRank rank]

instance Show Position where
    show = unlines . map showRow . reverse . getRows

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

otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

isColor :: Color -> Piece -> Bool
isColor color (_, pieceColor) = color == pieceColor

shortFile :: File -> Char
shortFile FileA = 'a'
shortFile FileB = 'b'
shortFile FileC = 'c'
shortFile FileD = 'd'
shortFile FileE = 'e'
shortFile FileF = 'f'
shortFile FileG = 'g'
shortFile FileH = 'h'

shortRank :: Rank -> Char
shortRank Rank1 = '1'
shortRank Rank2 = '2'
shortRank Rank3 = '3'
shortRank Rank4 = '4'
shortRank Rank5 = '5'
shortRank Rank6 = '6'
shortRank Rank7 = '7'
shortRank Rank8 = '8'

pieceToChar :: Piece -> Char
pieceToChar (Pawn, color) = colorize 'P' color
pieceToChar (Knight, color) = colorize 'N' color
pieceToChar (Bishop, color) = colorize 'B' color
pieceToChar (Rook, color) = colorize 'R' color
pieceToChar (Queen, color) = colorize 'Q' color
pieceToChar (King, color) = colorize 'K' color

colorize :: Char -> Color -> Char
colorize c color = if color == Black then toLower c else c

showSquare :: Square -> String
showSquare Nothing = " "
showSquare (Just piece) = [pieceToChar piece]

showRow :: [Square] -> String
showRow = unwords . map showSquare

startRows :: [[Square]]
startRows = [backRow White, pawnRow White, emptyRow, emptyRow, emptyRow, emptyRow, pawnRow Black, backRow Black]

backRow :: Color -> [Square]
backRow color = map (\chessman -> Just (chessman, color)) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawnRow :: Color -> [Square]
pawnRow color = replicate 8 $ Just (Pawn, color)

emptyRow :: [Square]
emptyRow = replicate 8 Nothing

getRows :: Position -> [[Square]]
getRows (Position vector) = map (\n -> V.toList $ V.slice (n*8) 8 vector) [0..7]

createCell :: File -> Rank -> Cell
createCell file rank = Cell (file, rank)

allCells :: [Cell]
allCells = enumFromTo minBound maxBound

getSquare :: Position -> Cell -> Square
getSquare (Position vector) cell = vector V.! fromEnum cell

hasPiece :: Position -> Cell -> Bool
hasPiece = hasPieceBy (const True)

hasPieceOfType :: Piece -> Position -> Cell -> Bool
hasPieceOfType piece = hasPieceBy (== piece)

hasPieceOfColor :: Color -> Position -> Cell -> Bool
hasPieceOfColor color = hasPieceBy (isColor color)

hasPieceBy :: (Piece -> Bool) -> Position -> Cell -> Bool
hasPieceBy predicate pos cell = maybe False predicate (getSquare pos cell)

cellsAndSquares :: Position -> [(Cell, Square)]
cellsAndSquares pos = map (\cell -> (cell, getSquare pos cell)) allCells 

cellsAndPieces :: Position -> [(Cell, Piece)]
cellsAndPieces = mapMaybe (\(cell, square) -> fmap ((,) cell) square) . cellsAndSquares

piecesOfColor :: Color -> Position -> [(Cell, Piece)]
piecesOfColor color pos = filter (isColor color . snd) (cellsAndPieces pos)

emptyPosition :: Position
emptyPosition = createPosition $ repeat Nothing

standardPosition :: Position
standardPosition = createPosition $ concat startRows

createPosition :: [Square] -> Position
createPosition = Position . V.fromListN 64

updatePosition :: Position -> [(Cell, Square)] -> Position
updatePosition (Position vector) updates = Position $ vector V.// map (first fromEnum) updates
