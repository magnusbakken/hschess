module Chesskel.Data (
    Piece,
    Chessman (..),
    Color (..),
    Square,
    Rank (..),
    File (..),
    Cell (..),
    Move (..),
    PromotionTarget (..),
    MoveContext (..),
    PositionContext (..),
    GameContext (..),
    getSquare,
    hasPiece,
    hasPieceOfType,
    isCheckmate,
    isStalemate,
    emptyPosition,
    startPosition,
    makeMove,
    makeNonPromotionMove,
    makePromotion,
    isLegalMove,
    isLegalNonPromotionMove,
    isLegalPromotion,
    findAllLegalMoves,
    startStandardGame,
    startGame,
    playMove,
    playNonPromotionMove,
    playPromotion,
    resign,
    draw
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.List as L
import Data.Maybe
import qualified Data.Vector as V

data Chessman = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
type Piece = (Chessman, Color)
type Square = Maybe Piece
newtype Position = Position (V.Vector Square) deriving (Eq)

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8 deriving (Eq, Ord, Bounded, Show)
data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH deriving (Eq, Ord, Bounded, Show)
newtype Cell = Cell (File, Rank) deriving (Eq, Ord, Bounded)
newtype Move = Move (Cell, Cell) deriving (Eq, Ord, Bounded)
data PromotionTarget = PKnight | PBishop | PRook | PQueen deriving (Eq, Show)
data CastlingData = CD {
    castleRookFromCell :: Cell,
    castleRookToCell :: Cell,
    castlingSpec :: Castling
} deriving (Eq)

data MoveContext = MC {
    mainFromCell :: Cell,
    mainToCell :: Cell,
    mainPiece :: Piece,
    player :: Color,
    isCapture :: Bool,
    enPassantCell :: Maybe Cell,
    castlingData :: Maybe CastlingData,
    promotionTarget :: Maybe PromotionTarget
} deriving (Eq)

data CastlingDirection = Queenside | Kingside deriving (Eq, Show)
type Castling = (CastlingDirection, Color)
data PositionContext = PC {
    position :: Position,
    currentPlayer :: Color,
    castlingRights :: [Castling],
    moveCount :: Int,
    halfMoveClock :: Int,
    previousEnPassantCell :: Maybe Cell
} deriving (Eq)

data Result = WhiteWin | Draw | BlackWin | Ongoing deriving (Eq)

data HeaderData = HD {
    eventHeader :: String,
    siteHeader :: String,
    dateHeader :: String,
    roundHeader :: String,
    whiteHeader :: String,
    blackHeader :: String,
    resultHeader :: Result
} deriving (Eq)

data ExtraHeader = EH { headerName :: String, headerValue :: String } deriving (Eq)

data GameContext = GC {
    currentPosition :: PositionContext,
    positions :: [PositionContext],
    moves :: [MoveContext],
    mainHeaderData :: HeaderData,
    extraHeaderData :: [ExtraHeader]
} deriving (Eq)

data MoveError =
    NoPieceAtSourceSquare |
    PieceCannotReachSquare |
    MoveWouldLeaveKingInCheck |
    PromotionIsNeeded |
    PromotionIsNotNeeded |
    DoesNotHaveCastlingRights |
    CastlingIsNotPossible deriving (Eq, Show)

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

instance Enum Move where
    fromEnum (Move (fromCell, toCell)) = (fromEnum fromCell * 64) + fromEnum toCell
    toEnum n
        | n >= 0 && n < 64*64 = let (q, r) = n `quotRem` 64 in createMove (toEnum q) (toEnum r)
        | otherwise = error $ "tag " ++ show n ++ " is outside of bounds (0, 4095)"

instance Show Cell where
    show (Cell (file, rank)) = [shortFile file, shortRank rank]

instance Show Move where
    show (Move (fromCell, toCell)) = show fromCell ++ "-" ++ show toCell

instance Show Position where
    show = unlines . map showRow . reverse . getRows

instance Show PositionContext where
    show pc = shows (position pc) ("\n" ++ showPlayerToMove pc)

instance Show Result where
    show WhiteWin = "1-0"
    show Draw = "1/2-1/2"
    show BlackWin = "0-1"
    show Ongoing = "*"

instance Show HeaderData where
    show headerData =
        "[Event \"" ++ eventHeader headerData ++ "\"]\n" ++
        "[Site \"" ++ siteHeader headerData ++ "\"]\n" ++
        "[Date \"" ++ dateHeader headerData ++ "\"]\n" ++
        "[Round \"" ++ roundHeader headerData ++ "\"]\n" ++
        "[White \"" ++ whiteHeader headerData ++ "\"]\n" ++
        "[Black \"" ++ blackHeader headerData ++ "\"]\n" ++
        "[Result \"" ++ show (resultHeader headerData) ++ "\"]"

instance Show ExtraHeader where
    show (EH { headerName = name, headerValue = value }) = "[" ++ name ++ " \"" ++ value ++ "\"]"

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

allCastlingRights :: [Castling]
allCastlingRights = [
    (Queenside, White),
    (Kingside, White),
    (Queenside, Black),
    (Kingside, Black)]

allPromotionTargets :: [PromotionTarget]
allPromotionTargets = [PKnight, PBishop, PRook, PQueen]

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

getPieceForPromotionTarget :: Color -> PromotionTarget -> Piece
getPieceForPromotionTarget color PKnight = (Knight, color)
getPieceForPromotionTarget color PBishop = (Bishop, color)
getPieceForPromotionTarget color PRook = (Rook, color)
getPieceForPromotionTarget color PQueen = (Queen, color)

showPiece :: Piece -> String
showPiece (Pawn, color) = colorize "P" color
showPiece (Knight, color) = colorize "N" color
showPiece (Bishop, color) = colorize "B" color
showPiece (Rook, color) = colorize "R" color
showPiece (Queen, color) = colorize "Q" color
showPiece (King, color) = colorize "K" color

colorize :: String -> Color -> String
colorize s color = if color == Black then map toLower s else s

showSquare :: Square -> String
showSquare Nothing = " "
showSquare (Just piece) = showPiece piece

showRow :: [Square] -> String
showRow = unwords . map showSquare

showPlayerToMove :: PositionContext -> String
showPlayerToMove PC { currentPlayer = White } = "White to move"
showPlayerToMove PC { currentPlayer = Black } = "Black to move"

emptyPosition :: Position
emptyPosition = Position $ V.fromListN 64 $ repeat Nothing

startPosition :: PositionContext
startPosition = PC {
    position = Position $ V.fromListN 64 $ concat startRows,
    currentPlayer = White,
    castlingRights = allCastlingRights,
    moveCount = 0,
    halfMoveClock = 0,
    previousEnPassantCell = Nothing
}

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

createMove :: Cell -> Cell -> Move
createMove fromCell toCell = Move (fromCell, toCell)

allCells :: [Cell]
allCells = enumFromTo minBound maxBound

getSquare :: Position -> Cell -> Square
getSquare (Position vector) cell = vector V.! fromEnum cell

hasPiece :: Position -> Cell -> Bool
hasPiece pos cell = isJust $ getSquare pos cell

hasPieceOfType :: Piece -> Position -> Cell -> Bool
hasPieceOfType piece pos cell = maybe False (== piece) (getSquare pos cell)

cellsAndSquares :: Position -> [(Cell, Square)]
cellsAndSquares pos = map (\cell -> (cell, getSquare pos cell)) allCells 

cellsAndPieces :: Position -> [(Cell, Piece)]
cellsAndPieces = mapMaybe (\(cell, square) -> fmap ((,) cell) square) . cellsAndSquares

isColor :: Color -> Piece -> Bool
isColor color (_, pieceColor) = color == pieceColor

hasPieceOfColor :: Color -> Position -> Cell -> Bool
hasPieceOfColor color pos cell = maybe False (isColor color) (getSquare pos cell)

cellHasPiece :: Position -> Cell -> Bool
cellHasPiece pos cell = isJust $ getSquare pos cell

piecesOfColor :: Color -> Position -> [(Cell, Piece)]
piecesOfColor color pos = filter (isColor color . snd) (cellsAndPieces pos)

makeMove :: PositionContext -> Move -> Maybe PromotionTarget -> Either MoveError PositionContext
makeMove pc move mpt = fmap (movePiece pc) (getMoveContext pc move mpt)

makeNonPromotionMove :: PositionContext -> Move -> Either MoveError PositionContext
makeNonPromotionMove pc move = makeMove pc move Nothing

makePromotion :: PositionContext -> Move -> PromotionTarget -> Either MoveError PositionContext
makePromotion pc move pt = makeMove pc move (Just pt)

getMoveContext :: PositionContext -> Move -> Maybe PromotionTarget -> Either MoveError MoveContext
getMoveContext pc (Move (fromCell, toCell)) mpt = do
    piece <- maybeToEither NoPieceAtSourceSquare $ getSquare (position pc) fromCell
    let move = createMove fromCell toCell
    cd <- getCastlingDataIfNecessary pc piece move
    pt <- getPromotionOrError piece move mpt
    let mc = MC {
        mainFromCell = fromCell,
        mainToCell = toCell,
        mainPiece = piece,
        player = currentPlayer pc,
        isCapture = isMoveCapture (position pc) move,
        enPassantCell = getEnPassantCell pc piece move,
        castlingData = cd,
        promotionTarget = pt
    }
    maybe (Right mc) Left (getMoveError pc mc)

getCastlingDataIfNecessary :: PositionContext -> Piece -> Move -> Either MoveError (Maybe CastlingData)
getCastlingDataIfNecessary pc piece move =
    maybe (Right Nothing) (fmap Just . getCastlingDataOrError pc) (getCastling piece move)

getCastlingDataOrError :: PositionContext -> Castling -> Either MoveError CastlingData
getCastlingDataOrError pc castling
    | castling `notElem` castlingRights pc = Left DoesNotHaveCastlingRights
    | otherwise = maybeToEither CastlingIsNotPossible $ do
        let (Move (fromCell, toCell)) = getRookCastlingMove castling
        guard $ validateKingForCastling pc castling
        guard $ validateRookForCastling pc (createMove fromCell toCell)
        return $ CD fromCell toCell castling

-- Chess960 incompatible
getCastling :: Piece -> Move -> Maybe Castling
getCastling (King, color) (Move (fromCell, toCell))
    | color == White && fromCell == e1 && toCell == c1 = Just (Queenside, White)
    | color == White && fromCell == e1 && toCell == g1 = Just (Kingside, White)
    | color == Black && fromCell == e8 && toCell == c8 = Just (Queenside, Black)
    | color == Black && fromCell == e8 && toCell == g8 = Just (Kingside, Black)
    | otherwise = Nothing
getCastling _ _ = Nothing

validateKingForCastling :: PositionContext -> Castling -> Bool
validateKingForCastling pc castling
  | isKingInCheck current (position pc) = False
  | any (hasPiece (position pc)) kingJourneyCells = False
  | any (anyPieceAttacks opponent (position pc)) kingJourneyCells = False
  | otherwise = True
    where current = currentPlayer pc
          opponent = otherColor current
          kingJourneyCells = kingCastlingJourneyCells castling

-- Chess960 incompatible
kingCastlingJourneyCells :: Castling -> [Cell]
kingCastlingJourneyCells (Queenside, White) = [d1, c1]
kingCastlingJourneyCells (Kingside, White) = [f1, g1]
kingCastlingJourneyCells (Queenside, Black) = [d8, c8]
kingCastlingJourneyCells (Kingside, Black) = [f8, g8]

-- Chess960 incompatible
getRookCastlingMove :: Castling -> Move
getRookCastlingMove (Queenside, White) = createMove a1 d1
getRookCastlingMove (Kingside, White) = createMove h1 f1
getRookCastlingMove (Queenside, Black) = createMove a8 d8
getRookCastlingMove (Kingside, Black) = createMove h8 f8

validateRookForCastling :: PositionContext -> Move -> Bool
validateRookForCastling pc (Move (fromCell, toCell)) =
    maybe False validate (getSquare (position pc) fromCell) where
    validate (Rook, col) = let move = createMove fromCell toCell in
                           pieceCanReach pc (Rook, col) move &&
                           hasClearPath (position pc) (Rook, col) move
    validate _ = False

getEnPassantCell :: PositionContext -> Piece -> Move -> Maybe Cell
getEnPassantCell (PC { previousEnPassantCell = Just cell }) (Pawn, color) (Move (_, toCell))
    | cell == toCell = Just $ getCapturedPawnCell color cell
    | otherwise = Nothing
    where
        getCapturedPawnCell White (Cell (file, _)) = Cell (file, Rank5)
        getCapturedPawnCell Black (Cell (file, _)) = Cell (file, Rank4)
getEnPassantCell _ _ _ = Nothing

getPromotionOrError :: Piece -> Move -> Maybe PromotionTarget -> Either MoveError (Maybe PromotionTarget)
getPromotionOrError piece move mpt
    | isPromotionNeeded piece move = Just <$> maybeToEither PromotionIsNeeded mpt
    | isJust mpt = Left PromotionIsNotNeeded -- Promotion not needed but PromotionTarget given. We regard this as an error.
    | otherwise = Right Nothing

isPromotionNeeded :: Piece -> Move -> Bool
isPromotionNeeded (Pawn, White) (Move (Cell (_, Rank7), Cell (_, Rank8))) = True
isPromotionNeeded (Pawn, Black) (Move (Cell (_, Rank2), Cell (_, Rank1))) = True
isPromotionNeeded _ _ = False

movePiece :: PositionContext -> MoveContext -> PositionContext
movePiece pc mc =
    let move = createMove (mainFromCell mc) (mainToCell mc)
        piece = mainPiece mc in
    PC {
        position = updatePosition mc (position pc),
        currentPlayer = otherColor (currentPlayer pc),
        castlingRights = updateCastlingRights (castlingRights pc) move,
        moveCount = 1 + moveCount pc,
        halfMoveClock = updateHalfMoveClock (halfMoveClock pc) (position pc) piece move,
        previousEnPassantCell = updatePreviousEnPassantCell piece move
    }

updatePosition :: MoveContext -> Position -> Position
updatePosition mc (Position vector) = Position $ vector V.// map (first fromEnum) (getPositionUpdates mc)

getPositionUpdates :: MoveContext -> [(Cell, Square)]
getPositionUpdates mc =
  let mpt = fmap (getPieceForPromotionTarget (player mc)) (promotionTarget mc) in
  concat [getMainPositionUpdates (mainFromCell mc) (mainToCell mc) (mainPiece mc) mpt,
          getCastlingRookPositionUpdates (castlingData mc),
          getEnPassantUpdates (enPassantCell mc)]

getMainPositionUpdates :: Cell -> Cell -> Piece -> Square -> [(Cell, Square)]
getMainPositionUpdates fromCell toCell piece mpt = [(fromCell, Nothing), (toCell, Just $ fromMaybe piece mpt)]

getCastlingRookPositionUpdates :: Maybe CastlingData -> [(Cell, Square)]
getCastlingRookPositionUpdates (Just cd) =
    let (_, color) = castlingSpec cd in
    [(castleRookFromCell cd, Nothing), (castleRookToCell cd, Just (Rook, color))]
getCastlingRookPositionUpdates Nothing = []

getEnPassantUpdates :: Maybe Cell -> [(Cell, Square)]
getEnPassantUpdates = map (flip (,) Nothing) . maybeToList

updateCastlingRights :: [Castling] -> Move -> [Castling]
updateCastlingRights cr (Move (fromCell, _))
    | null cr = []
    | fromCell == e1 = removeRights White [Queenside, Kingside]
    | fromCell == a1 = removeRights White [Queenside]
    | fromCell == h1 = removeRights White [Kingside]
    | fromCell == e8 = removeRights Black [Queenside, Kingside]
    | fromCell == a8 = removeRights Black [Queenside]
    | fromCell == h8 = removeRights Black [Kingside]
    | otherwise = cr where
        removeRights color sides = cr L.\\ map (\side -> (side, color)) sides

updateHalfMoveClock :: Int -> Position -> Piece -> Move -> Int
updateHalfMoveClock n pos piece move =
    if needsReset piece move then 0 else succ n where
        needsReset (Pawn, _) _ = True
        needsReset _ m = isMoveCapture pos m

updatePreviousEnPassantCell :: Piece -> Move -> Maybe Cell
updatePreviousEnPassantCell (Pawn, White) (Move (Cell (file, Rank2), Cell (_, Rank4))) =
    Just (createCell file Rank3)
updatePreviousEnPassantCell (Pawn, Black) (Move (Cell (file, Rank7), Cell (_, Rank5))) =
    Just (createCell file Rank6)
updatePreviousEnPassantCell _ _ = Nothing

getMoveError :: PositionContext -> MoveContext -> Maybe MoveError
getMoveError pc mc
    | not $ pieceCanReach pc piece move = Just PieceCannotReachSquare
    | not $ hasClearPath (position pc) piece move = Just PieceCannotReachSquare
    | isKingInCheck (currentPlayer pc) (position (movePiece pc mc)) = Just MoveWouldLeaveKingInCheck
    | otherwise = Nothing where
        move = createMove (mainFromCell mc) (mainToCell mc)
        piece = mainPiece mc

pieceCanReach :: PositionContext -> Piece -> Move -> Bool
pieceCanReach pc (Pawn, color) move = pawnCanReach color (position pc) move || isValidEnPassantCapture pc move
pieceCanReach pc (King, color) move = kingCanReach color pc move
pieceCanReach _ (chessman, _) move = chessmanCanReach chessman move

pieceAttacks :: Position -> Piece -> Move -> Bool
pieceAttacks pos (Pawn, color) move = isValidPawnCapture color pos move -- TODO: Does not account for en passant.
pieceAttacks _ (King, _) move = kingCanReachWithoutCastling move
pieceAttacks pos (chessman, color) move = chessmanCanReach chessman move && hasClearPath pos (chessman, color) move

chessmanCanReach :: Chessman -> Move -> Bool
chessmanCanReach Bishop = bishopCanReach
chessmanCanReach Knight = knightCanReach
chessmanCanReach Rook = rookCanReach
chessmanCanReach Queen = queenCanReach
chessmanCanReach Pawn = error "Use pawnCanReach instead for pawns"
chessmanCanReach King = error "Use kingCanReach instead for pawns"

hasClearPath :: Position -> Piece -> Move -> Bool
hasClearPath pos (chessman, color) (Move (fromCell, toCell))
    | hasPieceOfColor color pos toCell = False
    | chessman == Knight = True -- Nothing stands in the way of a knight.
    | otherwise = not $ any (cellHasPiece pos) (interpolateCells fromCell toCell)

interpolateCells :: Cell -> Cell -> [Cell]
interpolateCells (Cell (fromFile, fromRank)) (Cell (toFile, toRank))
    | fromFile == toFile = map (createCell fromFile) ranks
    | fromRank == toRank = map (`createCell` fromRank) files
    | otherwise = zipWith createCell files ranks
    where
        files = interpolate fromFile toFile
        ranks = interpolate fromRank toRank

interpolate :: (Enum n, Ord n) => n -> n -> [n]
interpolate from to
    | from == to = []
    | from > to = reverse (interpolate to from)
    | otherwise = [succ from..pred to]

pawnCanReach :: Color -> Position -> Move -> Bool
pawnCanReach color pos move =
    isValidRegularPawnMove color pos move ||
    isValidDoublePawnMove color pos move ||
    isValidPawnCapture color pos move

isValidRegularPawnMove :: Color -> Position -> Move -> Bool
isValidRegularPawnMove color pos (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile &&
    toRank == expectedRank color fromRank &&
    not (cellHasPiece pos (createCell toFile toRank))
    where
        -- TODO: this will give an error if there's somehow a pawn on the first/last rank.
        expectedRank White = succ
        expectedRank Black = pred

isValidDoublePawnMove :: Color -> Position -> Move -> Bool
isValidDoublePawnMove color pos (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile &&
    checkRanks color fromRank toRank &&
    not (cellHasPiece pos (createCell toFile toRank)) &&
    hasClearPath pos (Pawn, color) (createMove (createCell fromFile fromRank) (createCell toFile toRank))
    where
        checkRanks White Rank2 Rank4 = True
        checkRanks Black Rank7 Rank5 = True
        checkRanks _ _ _  = False

isValidPawnCapture :: Color -> Position -> Move -> Bool
isValidPawnCapture color pos (Move (fromCell, toCell)) =
    isValidPawnCapture' color (createMove fromCell toCell) &&
    hasPieceOfColor Black pos toCell

isValidEnPassantCapture :: PositionContext -> Move -> Bool
isValidEnPassantCapture pc (Move (fromCell, toCell)) =
    previousEnPassantCell pc == Just toCell &&
    isValidPawnCapture' (currentPlayer pc) (createMove fromCell toCell)

isValidPawnCapture' :: Color -> Move -> Bool
isValidPawnCapture' color (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    toFile `elem` adjacentFiles fromFile && toRank == expectedRank color fromRank
    where
        -- TODO: this will give an error if there's somehow a pawn on the first/last rank
        expectedRank White = succ
        expectedRank Black = pred
        adjacentFiles FileA = [FileB]
        adjacentFiles FileH = [FileG]
        adjacentFiles file = [pred file, succ file]

kingCanReach :: Color -> PositionContext -> Move -> Bool
kingCanReach color pc move =
    let checkCastling = getCastling (King, color) move in
    maybe (kingCanReachWithoutCastling move) (kingCanReachByCastling pc) checkCastling

kingCanReachByCastling :: PositionContext -> Castling -> Bool
kingCanReachByCastling pc castling = isRight $ getCastlingDataOrError pc castling

kingCanReachWithoutCastling :: Move -> Bool
kingCanReachWithoutCastling (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    abs (fromEnum fromFile - fromEnum toFile) <= 1 && abs (fromEnum fromRank - fromEnum toRank) <= 1

bishopCanReach :: Move -> Bool
bishopCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    abs (fromFileN - toFileN) == abs (fromRankN - toRankN)
    where
        fromFileN = fromEnum fromFile
        fromRankN = fromEnum fromRank
        toFileN = fromEnum toFile
        toRankN = fromEnum toRank

knightCanReach :: Move -> Bool
knightCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    (abs (fromFileN - toFileN) == 1 && abs (fromRankN - toRankN) == 2) ||
    (abs (fromFileN - toFileN) == 2 && abs (fromRankN - toRankN) == 1)
    where
        fromFileN = fromEnum fromFile
        fromRankN = fromEnum fromRank
        toFileN = fromEnum toFile
        toRankN = fromEnum toRank

rookCanReach :: Move -> Bool
rookCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile || fromRank == toRank

queenCanReach :: Move -> Bool
queenCanReach move = rookCanReach move || bishopCanReach move

isKingInCheck :: Color -> Position -> Bool
isKingInCheck color pos = anyPieceAttacks (otherColor color) pos (findKing color pos)

isCheckmate :: PositionContext -> Bool
isCheckmate pc = null (findAllLegalMoves pc) && isKingInCheck (currentPlayer pc) (position pc)

isStalemate :: PositionContext -> Bool
isStalemate pc = null (findAllLegalMoves pc) && not (isKingInCheck (currentPlayer pc) (position pc))

findKing :: Color -> Position -> Cell
findKing color pos = fst . head $ filter isKing (piecesOfColor color pos) where
    isKing (_, (King, _)) = True
    isKing _ = False

anyPieceAttacks :: Color -> Position -> Cell -> Bool
anyPieceAttacks color pos targetCell =
  let pieces = piecesOfColor color pos
      f (cell, piece) = (piece, createMove cell targetCell)
      attackingMoves = map f pieces in
  any (uncurry (pieceAttacks pos)) attackingMoves

isMoveCapture :: Position -> Move -> Bool
isMoveCapture pos (Move (_, toCell)) = cellHasPiece pos toCell

isMovePromotion :: Piece -> Move -> Bool
isMovePromotion (Pawn, White) (Move (_, Cell (_, Rank8))) = True
isMovePromotion (Pawn, Black) (Move (_, Cell (_, Rank1))) = True
isMovePromotion _ _ = False

getPossiblePromotionTargets :: Piece -> Move -> [Maybe PromotionTarget]
getPossiblePromotionTargets piece move
    | isMovePromotion piece move = map Just allPromotionTargets
    | otherwise = [Nothing]

findLegalMoves :: PositionContext -> (Cell, Piece) -> [MoveContext]
findLegalMoves pc (fromCell, piece) = do
    toCell <- allCells
    pt <- getPossiblePromotionTargets piece (createMove fromCell toCell)
    rights [getMoveContext pc (createMove fromCell toCell) pt]

findAllLegalMoves :: PositionContext -> [MoveContext]
findAllLegalMoves pc = concatMap (findLegalMoves pc) (piecesOfColor (currentPlayer pc) (position pc))

extractMainMove :: MoveContext -> Move
extractMainMove mc = createMove (mainFromCell mc) (mainToCell mc)

isLegalMove :: PositionContext -> Move -> Maybe PromotionTarget -> Bool
isLegalMove pc move mpt = isRight $ getMoveContext pc move mpt

isLegalNonPromotionMove :: PositionContext -> Move -> Bool
isLegalNonPromotionMove pc move = isLegalMove pc move Nothing

isLegalPromotion :: PositionContext -> Move -> PromotionTarget -> Bool
isLegalPromotion pc move pt = isLegalMove pc move (Just pt)

unknownHeaderData :: HeaderData
unknownHeaderData = HD {
    eventHeader = "Haskell chess",
    siteHeader = "Unknown",
    dateHeader = "Unknown", -- We'd fill in today's date here, but then we'd have to be in the IO monad.
    roundHeader = "Unknown",
    whiteHeader = "Unknown",
    blackHeader = "Unknown",
    resultHeader = Ongoing
}

startStandardGame :: GameContext
startStandardGame = startGame startPosition unknownHeaderData

startGame :: PositionContext -> HeaderData -> GameContext
startGame pc hd = GC {
    currentPosition = pc,
    positions = [],
    moves = [],
    mainHeaderData = hd,
    extraHeaderData = []
}

playMove :: Move -> Maybe PromotionTarget -> GameContext -> Either MoveError GameContext
playMove move mpt gc = do
    mc <- getMoveContext (currentPosition gc) move mpt
    return $ updateGameContext mc (movePiece (currentPosition gc) mc) gc

playNonPromotionMove :: Move -> GameContext -> Either MoveError GameContext
playNonPromotionMove move = playMove move Nothing

playPromotion :: Move -> PromotionTarget -> GameContext -> Either MoveError GameContext
playPromotion move pt = playMove move (Just pt)

updateGameContext :: MoveContext -> PositionContext -> GameContext -> GameContext
updateGameContext mc pc gc = gc {
    currentPosition = pc,
    positions = currentPosition gc:positions gc,
    moves = mc:moves gc,
    mainHeaderData = updateHeaderDataIfFinished pc (mainHeaderData gc)
}

resign :: Color -> GameContext -> GameContext
resign color gc = gc {
    mainHeaderData = setWinner (otherColor color) (mainHeaderData gc)
}

draw :: GameContext -> GameContext
draw gc = gc {
    mainHeaderData = setDraw (mainHeaderData gc)
}

updateHeaderDataIfFinished :: PositionContext -> HeaderData -> HeaderData
updateHeaderDataIfFinished pc hd
    | isCheckmate pc = setWinner (otherColor (currentPlayer pc)) hd
    | isStalemate pc = setDraw hd
    | otherwise = hd

setWinner :: Color -> HeaderData -> HeaderData
setWinner White = setResult WhiteWin
setWinner Black = setResult BlackWin

setDraw :: HeaderData -> HeaderData
setDraw = setResult Draw

setResult :: Result -> HeaderData -> HeaderData
setResult result hd = hd { resultHeader = result }

-- Generic utils functions
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

isRight :: Either e a -> Bool -- Added to Data.Either in recent GHC version.
isRight (Right _) = True
isRight (Left _) = False
