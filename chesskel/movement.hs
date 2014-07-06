module Chesskel.Movement (
    Castling,
    CastlingData (..),
    CastlingDirection (..),
    Move (..),
    MoveContext (..),
    MoveError (..),
    PositionContext (..),
    PromotionTarget (..),
    UnderspecifiedMove (..),
    createMove,
    isCheckmate,
    isLegalMove,
    isLegalNonPromotionMove,
    isLegalPromotion,
    isStalemate,
    findAllLegalMoves,
    makeMove,
    makeNonPromotionMove,
    makeUnderspecifiedMove,
    startPosition,
) where

import Chesskel.Board
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import qualified Data.Set as S

newtype Move = Move (Cell, Cell) deriving (Eq, Ord, Bounded)
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

data UnderspecifiedMove = CastleMove CastlingDirection | UM {
    knownToCell :: Cell,
    knownPiece :: Piece,
    knownFromFile :: Maybe File,
    knownFromRank :: Maybe Rank,
    knownPromotionTarget :: Maybe PromotionTarget
} deriving (Eq)

data PromotionTarget = PKnight | PBishop | PRook | PQueen deriving (Eq, Show)
data CastlingData = CD {
    castleRookFromCell :: Cell,
    castleRookToCell :: Cell,
    castlingSpec :: Castling
} deriving (Eq)

data CastlingDirection = Kingside | Queenside deriving (Eq, Ord)
type Castling = (CastlingDirection, Color)
data PositionContext = PC {
    position :: Position,
    currentPlayer :: Color,
    castlingRights :: S.Set Castling,
    previousEnPassantCell :: Maybe Cell,
    halfMoveClock :: Int,
    moveCount :: Int
} deriving (Eq)

data MoveError =
    NoPieceAtSourceSquare |
    PieceCannotReachSquare |
    MoveWouldLeaveKingInCheck |
    PromotionIsNeeded |
    PromotionIsNotNeeded |
    DoesNotHaveCastlingRights Castling |
    CastlingIsNotPossible Castling |
    InsufficientDisambiguation [Cell] deriving (Eq, Show)

instance Enum Move where
    fromEnum (Move (fromCell, toCell)) = (fromEnum fromCell * 64) + fromEnum toCell
    toEnum n
        | n >= 0 && n < 64*64 = let (q, r) = n `quotRem` 64 in createMove (toEnum q) (toEnum r)
        | otherwise = error $ "tag " ++ show n ++ " is outside of bounds (0, 4095)"

instance Show CastlingDirection where
    show Kingside = "O-O"
    show Queenside = "O-O-O"

instance Show Move where
    show (Move (fromCell, toCell)) = show fromCell ++ "-" ++ show toCell

instance Show PositionContext where
    show pc = shows (position pc) ("\n" ++ showPlayerToMove pc)

allPromotionTargets :: [PromotionTarget]
allPromotionTargets = [PKnight, PBishop, PRook, PQueen]

allCastlingRights :: S.Set Castling
allCastlingRights = S.fromList [
    (Kingside, White),
    (Kingside, Black),
    (Queenside, White),
    (Queenside, Black)]

showPlayerToMove :: PositionContext -> String
showPlayerToMove PC { currentPlayer = White } = "White to move"
showPlayerToMove PC { currentPlayer = Black } = "Black to move"

createMove :: Cell -> Cell -> Move
createMove fromCell toCell = Move (fromCell, toCell)

startPosition :: PositionContext
startPosition = PC {
    position = standardPosition,
    currentPlayer = White,
    castlingRights = allCastlingRights,
    previousEnPassantCell = Nothing,
    halfMoveClock = 0,
    moveCount = 1
}

getPieceForPromotionTarget :: Color -> PromotionTarget -> Piece
getPieceForPromotionTarget color PKnight = (Knight, color)
getPieceForPromotionTarget color PBishop = (Bishop, color)
getPieceForPromotionTarget color PRook = (Rook, color)
getPieceForPromotionTarget color PQueen = (Queen, color)

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
    | castling `S.notMember` castlingRights pc = Left (DoesNotHaveCastlingRights castling)
    | otherwise = maybeToEither (CastlingIsNotPossible castling) $ do
        let (Move (fromCell, toCell)) = rookCastlingMove castling
        guard $ validateKingForCastling pc castling
        guard $ validateRookForCastling pc (createMove fromCell toCell)
        return $ CD fromCell toCell castling

-- Chess960 incompatible
getCastling :: Piece -> Move -> Maybe Castling
getCastling (King, color) move = findCastlingMove (map (\c -> (c, kingCastlingMove c)) (S.toList allCastlingRights)) move color
getCastling _ _ = Nothing

findCastlingMove :: [(Castling, Move)] -> Move -> Color -> Maybe Castling
findCastlingMove [] _ _ = Nothing
findCastlingMove (((direction, color'), move') : xs) move color =
    if move == move' && color == color' then Just (direction, color) else findCastlingMove xs move color

validateKingForCastling :: PositionContext -> Castling -> Bool
validateKingForCastling pc castling
    | isKingInCheck current (position pc) = False
    | any (hasPiece (position pc)) kingJourneyCells = False
    | any (anyPieceAttacks opponent (position pc)) kingJourneyCells = False
    | otherwise = True where
        current = currentPlayer pc
        opponent = otherColor current
        kingJourneyCells = kingCastlingJourneyCells castling

-- Chess960 incompatible
kingCastlingMove :: Castling -> Move
kingCastlingMove castling = createMove (kingCastlingSource castling) (kingCastlingDestination castling)

-- Chess960 incompatible
kingCastlingSource :: Castling -> Cell
kingCastlingSource (_, White) = e1
kingCastlingSource (_, Black) = e8

-- Chess960 incompatible
kingCastlingDestination :: Castling -> Cell
kingCastlingDestination (Kingside, White) = g1
kingCastlingDestination (Kingside, Black) = g8
kingCastlingDestination (Queenside, White) = c1
kingCastlingDestination (Queenside, Black) = c8

-- Chess960 incompatible
kingCastlingJourneyCells :: Castling -> [Cell]
kingCastlingJourneyCells (Kingside, White) = [f1, g1]
kingCastlingJourneyCells (Kingside, Black) = [f8, g8]
kingCastlingJourneyCells (Queenside, White) = [d1, c1]
kingCastlingJourneyCells (Queenside, Black) = [d8, c8]

-- Chess960 incompatible
rookCastlingMove :: Castling -> Move
rookCastlingMove castling = createMove (rookCastlingSource castling) (rookCastlingDestination castling)

-- Chess960 incompatible
rookCastlingSource :: Castling -> Cell
rookCastlingSource (Kingside, White) = h1
rookCastlingSource (Kingside, Black) = h8
rookCastlingSource (Queenside, White) = a1
rookCastlingSource (Queenside, Black) = a8

-- Chess960 incompatible
rookCastlingDestination :: Castling -> Cell
rookCastlingDestination (Kingside, White) = f1
rookCastlingDestination (Kingside, Black) = f8
rookCastlingDestination (Queenside, White) = d1
rookCastlingDestination (Queenside, Black) = d8

validateRookForCastling :: PositionContext -> Move -> Bool
validateRookForCastling pc (Move (fromCell, toCell)) =
    maybe False validate (getSquare (position pc) fromCell) where
        validate (Rook, col) =
            let move = createMove fromCell toCell in
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
        position = updatePosition (position pc) (getPositionUpdates mc),
        currentPlayer = otherColor (currentPlayer pc),
        castlingRights = updateCastlingRights (castlingRights pc) move,
        previousEnPassantCell = updatePreviousEnPassantCell piece move,
        halfMoveClock = updateHalfMoveClock (halfMoveClock pc) (position pc) piece move,
        moveCount = updateMoveCount (moveCount pc) (currentPlayer pc)
    }

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

updateCastlingRights :: S.Set Castling -> Move -> S.Set Castling
updateCastlingRights cr (Move (fromCell, _))
    | S.null cr = cr
    | fromCell == e1 = removeRights [(Kingside, White), (Queenside, White)]
    | fromCell == h1 = removeRights [(Kingside, White)]
    | fromCell == a1 = removeRights [(Queenside, White)]
    | fromCell == e8 = removeRights [(Kingside, Black), (Queenside, Black)]
    | fromCell == h8 = removeRights [(Kingside, Black)]
    | fromCell == a8 = removeRights [(Queenside, Black)]
    | otherwise = cr where
        removeRights c = S.difference cr (S.fromList c)

updatePreviousEnPassantCell :: Piece -> Move -> Maybe Cell
updatePreviousEnPassantCell (Pawn, White) (Move (Cell (file, Rank2), Cell (_, Rank4))) =
    Just (createCell file Rank3)
updatePreviousEnPassantCell (Pawn, Black) (Move (Cell (file, Rank7), Cell (_, Rank5))) =
    Just (createCell file Rank6)
updatePreviousEnPassantCell _ _ = Nothing

updateHalfMoveClock :: Int -> Position -> Piece -> Move -> Int
updateHalfMoveClock n pos piece move =
    if needsReset piece move then 0 else succ n where
        needsReset (Pawn, _) _ = True
        needsReset _ m = isMoveCapture pos m

updateMoveCount :: Int -> Color -> Int
updateMoveCount n White = n
updateMoveCount n Black = succ n

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
chessmanCanReach King = error "Use kingCanReach instead for kings"

hasClearPath :: Position -> Piece -> Move -> Bool
hasClearPath pos (chessman, color) (Move (fromCell, toCell))
    | hasPieceOfColor color pos toCell = False
    | chessman == Knight = True -- Nothing stands in the way of a knight.
    | otherwise = not $ any (hasPiece pos) (interpolateCells fromCell toCell)

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
    not (hasPiece pos (createCell toFile toRank))
    where
        -- TODO: this will give an error if there's somehow a pawn on the first/last rank.
        expectedRank White = succ
        expectedRank Black = pred

isValidDoublePawnMove :: Color -> Position -> Move -> Bool
isValidDoublePawnMove color pos (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile &&
    checkRanks color fromRank toRank &&
    not (hasPiece pos (createCell toFile toRank)) &&
    hasClearPath pos (Pawn, color) (createMove (createCell fromFile fromRank) (createCell toFile toRank))
    where
        checkRanks White Rank2 Rank4 = True
        checkRanks Black Rank7 Rank5 = True
        checkRanks _ _ _  = False

isValidPawnCapture :: Color -> Position -> Move -> Bool
isValidPawnCapture color pos (Move (fromCell, toCell)) =
    isValidPawnCapture' color (createMove fromCell toCell) &&
    hasPieceOfColor (otherColor color) pos toCell

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
isMoveCapture pos (Move (_, toCell)) = hasPiece pos toCell

isMovePromotion :: Piece -> Move -> Bool
isMovePromotion (Pawn, White) (Move (_, Cell (_, Rank8))) = True
isMovePromotion (Pawn, Black) (Move (_, Cell (_, Rank1))) = True
isMovePromotion _ _ = False

-- Disambiguation for underspecified moves.
getSourceCell :: PositionContext -> UnderspecifiedMove -> Either MoveError Cell
getSourceCell (PC { currentPlayer = White }) (CastleMove _) = return e1
getSourceCell (PC { currentPlayer = Black }) (CastleMove _) = return e8
getSourceCell pc unspecMove =
    case createCell <$> knownFromFile unspecMove <*> knownFromRank unspecMove of
        Just c -> return c
        Nothing -> disambiguateSourceCell pc unspecMove

disambiguateSourceCell :: PositionContext -> UnderspecifiedMove -> Either MoveError Cell
disambiguateSourceCell pc unspecMove =
    let toCell = knownToCell unspecMove
        piece = knownPiece unspecMove
        mFromFile = knownFromFile unspecMove
        mFromRank = knownFromRank unspecMove in
    case findCandidateSourceCells pc toCell piece mFromFile mFromRank of
        [] -> Left NoPieceAtSourceSquare
        [single] -> Right single
        multiple -> Left $ InsufficientDisambiguation multiple

findCandidateSourceCells :: PositionContext -> Cell -> Piece -> Maybe File -> Maybe Rank -> [Cell]
findCandidateSourceCells pc toCell piece mFromFile mFromRank = do
    MC { mainFromCell = fromCell, mainToCell = toCell' } <- findAllLegalMoves pc
    guard $ toCell == toCell'
    let Cell (f, r) = fromCell
    guard $ maybe True (== f) mFromFile
    guard $ maybe True (== r) mFromRank
    guard $ hasPieceOfType piece (position pc) fromCell
    return fromCell

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

isLegalMove :: PositionContext -> Move -> Maybe PromotionTarget -> Bool
isLegalMove pc move mpt = isRight $ getMoveContext pc move mpt

isLegalNonPromotionMove :: PositionContext -> Move -> Bool
isLegalNonPromotionMove pc move = isLegalMove pc move Nothing

isLegalPromotion :: PositionContext -> Move -> PromotionTarget -> Bool
isLegalPromotion pc move pt = isLegalMove pc move (Just pt)

makeMove :: PositionContext -> Move -> Maybe PromotionTarget -> Either MoveError (MoveContext, PositionContext)
makeMove pc move mpt = (\mc -> (mc, movePiece pc mc)) <$> getMoveContext pc move mpt

makeUnderspecifiedMove :: PositionContext -> UnderspecifiedMove -> Either MoveError (MoveContext, PositionContext)
makeUnderspecifiedMove pc (CastleMove direction) =
    makeMove pc (kingCastlingMove (direction, currentPlayer pc)) Nothing
makeUnderspecifiedMove pc unspecMove = do
    fromCell <- getSourceCell pc unspecMove
    makeMove pc (createMove fromCell (knownToCell unspecMove)) (knownPromotionTarget unspecMove)

makeNonPromotionMove :: PositionContext -> Move -> Either MoveError (MoveContext, PositionContext)
makeNonPromotionMove pc move = makeMove pc move Nothing

makePromotion :: PositionContext -> Move -> PromotionTarget -> Either MoveError (MoveContext, PositionContext)
makePromotion pc move pt = makeMove pc move (Just pt)

-- Generic utils functions
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

isRight :: Either e a -> Bool -- Added to Data.Either in recent GHC version.
isRight (Right _) = True
isRight (Left _) = False
