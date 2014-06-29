module Chesskel.Movement (
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

import Chesskel.Data
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import qualified Data.List as L

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
        position = updatePosition (position pc) (getPositionUpdates mc),
        currentPlayer = otherColor (currentPlayer pc),
        castlingRights = updateCastlingRights (castlingRights pc) move,
        moveCount = 1 + moveCount pc,
        halfMoveClock = updateHalfMoveClock (halfMoveClock pc) (position pc) piece move,
        previousEnPassantCell = updatePreviousEnPassantCell piece move
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
isMoveCapture pos (Move (_, toCell)) = hasPiece pos toCell

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
