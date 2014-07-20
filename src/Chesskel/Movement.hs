{-|
Module      : Chesskel.Board
Description : Functions for chess positions, pieces and cells.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions related to moves, including promotions and move
errors. There are multiple move data models used for different purposes. If you
only want to analyze moves for specific positions, independent of their context
within a game, you can stick to this module. If you need to deal with entire
chess games, you have to consider the "Chesskel.Gameplay" module as well.
-}
module Chesskel.Movement (
    Move (..),
    MoveContext (..),
    UnderspecifiedMove (..),
    MinimalMove,
    PromotionTarget (..),
    CastlingData (..),
    CheckState (..),
    MoveAnnotation (..),
    MoveError (..),
    
    -- ** Move functions
    -- |Functions that change the position on the board by playing a move.
    
    makeMove,
    makePromotion,
    makeNonPromotionMove,
    makeMinimalMove,
    
    -- ** Move legality checking
    -- |Functions that find legal moves, or determine if given moves are legal.
    
    findAllLegalMoves,
    isLegalMove,
    isLegalPromotion,
    isLegalNonPromotionMove,
    isLegalMinimalMove,
    
    -- ** Game state checks
    -- |Functions that determine the current state of the position.
    
    isKingInCheck,
    isCheckmate,
    isStalemate,
    getCheckState,
    
    -- ** Other
    -- |Functions that don't fit into any of the above categories.
    
    createMove,
    createMinimalMove,
    randomMove,
) where

import Chesskel.Board
import Chesskel.Utils
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Test.QuickCheck.Gen

-- |A move is simply a pair of coordinates (cells). Except for promotion
--  targets for pawn moves, this is always sufficient to uniquely identify a
--  move.
newtype Move = Move (Cell, Cell) deriving (Eq, Ord, Bounded)

-- |A move context is a denormalized data structure that contains all the
--  information necessary to fully reconstruct a move, provided that the
--  position in which it was played is also available.
data MoveContext = MkMoveContext {
    -- |The cell the piece moved from. Equivalent to the first cell in the Move
    --  type.
    --
    --  If the move is a castling move, there are actually two pieces that are
    --  moving. In those cases the castlingData must be perused as well.
    mainFromCell :: Cell,
    
    -- |The cell the piece moved to. Equivalent to the second cell in the Move
    --  type.
    --
    --  If the move is a castling move, there are actually two pieces that are
    --  moving. In those cases the castlingData must be perused as well.
    mainToCell :: Cell,
    
    -- |The main piece that made the move.
    --
    --  If the move is a castling move, this is the king. The rook's position
    --  is included in the castlingData field.
    mainPiece :: Piece,
    
    -- |The player that made the move.
    player :: Color,
    
    -- |A value indicating whether the move was a capture.
    isCapture :: Bool,
    
    -- |If the move was an en passant capture, this is the cell that the
    --  captured pawn is on.
    --
    --  This is not to be confused with the previousEnPassantCell field on the
    --  PositionContext, which is the cell the capture can be made at. For
    --  instance, if white moves a pawn from e2 to e4, the PositionContext will
    --  contain e3. If black then captures en passant on e3, this field will
    --  have a value of e4.
    enPassantCell :: Maybe Cell,
    
    -- |If the move is a castling move, this will contain information about the
    --  castling.
    castlingData :: Maybe CastlingData,
    
    -- |If the move is a promotion, this will contain the promotion target.
    promotionTarget :: Maybe PromotionTarget,
    
    -- |An optional annotation for the move.
    moveAnnotation :: Maybe MoveAnnotation
} deriving (Eq)

-- |An annotation is an arbitrary piece of text associated with a move.
newtype MoveAnnotation = MkMoveAnnotation T.Text deriving (Eq, Ord)

-- |An underspecified move is a move that contains as little information as
--  possible about the move, while hopefully still being unambiguous. This is
--  the move specification model used in formats like PGN.
data UnderspecifiedMove =
    CastleMove CastlingDirection |
    MkUnderspecifiedMove {
    -- |The destination cell. This value must always be present.
    knownToCell :: Cell,
    
    -- |The chessman that made the move. This value must always be present.
    knownChessman :: Chessman,
    
    -- |The file of the source cell. This should only be specified if there are
    --  one or more other pieces of the same type on the same rank that
    --  could've made the move, or if the piece is a pawn and the move is a
    --  capture.
    knownFromFile :: Maybe File,
    
    -- |The rank of the source cell. This should only be specified if there are
    --  one or more other pieces of the same type on the same file that
    --  could've made the move.
    knownFromRank :: Maybe Rank,
    
    -- |Whether the move is a capture. This will not be validated when the move
    --  is played with 'makeMinimalMove', but is necessary for the move to
    --  correctly be marked as a capture when exporting the SAN.
    knownIsCapture :: Bool,
    
    -- |The promotion target, if the move is a pawn promotion move.
    knownPromotionTarget :: Maybe PromotionTarget,
    
    -- |The check state, if the move is a check or checkmate.
    knownCheckState :: Maybe CheckState
} deriving (Eq)

-- |A minimal move is a combination of an underspecified move and an optional
--  annotation.
type MinimalMove = (UnderspecifiedMove, Maybe MoveAnnotation)

-- |The pieces that a pawn may promote to.
data PromotionTarget = PKnight | PBishop | PRook | PQueen deriving (Eq, Show)

-- |A denormalized definition of castling data, containing the movement of the
--  rook as well as the actual castling specification.
data CastlingData = MkCastlingData {
    -- |The source cell of the rook.
    castleRookFromCell :: Cell,
    
    -- |The destination cell of the rook.
    castleRookToCell :: Cell,
    
    -- |The castling type (direction and color).
    castlingSpec :: Castling
} deriving (Eq)

-- |A check state is either a check or a checkmate. Mainly used to correctly
--  represent moves in SAN.
data CheckState = Check | Checkmate deriving (Eq, Show)

-- |Move errors occur whenever one of the makeMove functions is used and the
--  move was not legal.
--
--  Each error describes why the move was illegal, and some of them contain
--  extra data associated with the validation error.
data MoveError =
    -- |The move could not be played because there was no piece at the
    --  indicated source cell.
    NoPieceAtSourceSquare |
    
    -- |The move could not be played because the piece cannot reach the
    --  indicated destination cell.
    --
    --  This is applicable both when the piece couldn't possibly reach the
    --  cell, e.g. if an attempt is made to move from a1 to f8, or if the move
    --  could be made except for there being something blocking the movement.
    --  This could be either due to a friendly piece being in the destination
    --  cell, or a friendly or enemy piece in any of the intermediate cells.
    PieceCannotReachSquare |
    
    -- |The move could not be played because even though the piece can reach
    --  its destination, the move would leave the king in check.
    --
    --  This is applicable both when the king itself is moving into check (or
    --  failing to leave check), and when another piece is pinned to the king
    --  but attempts to move out of the way.
    MoveWouldLeaveKingInCheck |
    
    -- |The move is a pawn move to the last rank, but no promotion target was
    --  given.
    PromotionIsNeeded |
    
    -- |The move is not a pawn move to the last rank, but a promotion target
    --  was given anyway. This indicates sloppy logic in the client, so it's
    --  regarded as an error rather than being ignored.
    PromotionIsNotNeeded |
    
    -- |The king attempted to castle, but did not have the necessary castling
    --  rights. Includes the castling specification that was attempted.
    DoesNotHaveCastlingRights Castling |
    
    -- |The king attempted to castle, and had the necessary castling rights,
    --  but something is preventing the castling move.
    --
    --  This could be because a piece is blocking the king or rook from moving,
    --  or because one of the cells the king would travel through (including
    --  the start cell) is under attack. Includes the castling specification
    --  that was attempted.
    CastlingIsNotPossible Castling |
    
    -- |The move was insufficiently disambiguated, making it impossible to know
    --  which piece was supposed to be moving.
    -- 
    --  This is only applicable when 'makeMinimalMove' (or
    --  'Chesskel.Gameplay.playMinimalMove') is used. A list of possible source
    --  cells is included with the error.
    InsufficientDisambiguation [Cell] |
    
    -- |The move was otherwise legal, but the game is marked as finished.
    --
    --  Only the "Chesskel.Gameplay" module knows whether the game is finished,
    --  so only the 'Chesskel.Gameplay.playMove' function(s) in that module
    --  will ever return this error.
    GameIsFinished deriving (Eq, Show)

instance Enum Move where
    fromEnum (Move (fromCell, toCell)) = (fromEnum fromCell * 64) + fromEnum toCell
    toEnum n
        | n >= 0 && n < 64*64 =
            let (q, r) = n `quotRem` 64 in createMove (toEnum q) (toEnum r)
        | otherwise = error $ "tag " ++ show n ++ " is outside of bounds (0, 4095)"

instance Show Move where
    show (Move (fromCell, toCell)) = show fromCell ++ "-" ++ show toCell

instance Show MoveAnnotation where
    show (MkMoveAnnotation s) = "{" ++ T.unpack s ++ "}"

instance Show UnderspecifiedMove where
    showsPrec n (CastleMove direction) = showsPrec n direction
    showsPrec _ mv =
        showChessman . showFromFile . showFromRank . showCapture . showToCell . showPromotion . showCheck where
        showChessman = sChessman (knownChessman mv)
        showFromFile = maybe showEmpty sFile (knownFromFile mv)
        showFromRank = maybe showEmpty sRank (knownFromRank mv)
        showCapture = if knownIsCapture mv then showString "x" else showEmpty
        showToCell = let Cell (f, r) = knownToCell mv in sFile f . sRank r
        showPromotion = maybe showEmpty sPromotion (knownPromotionTarget mv)
        showCheck = maybe showEmpty sCheck (knownCheckState mv)
        sChessman King = showString "K"
        sChessman Queen = showString "Q"
        sChessman Rook = showString "R"
        sChessman Bishop = showString "B"
        sChessman Knight = showString "N"
        sChessman Pawn = showEmpty
        sFile FileA = showString "a"
        sFile FileB = showString "b"
        sFile FileC = showString "c"
        sFile FileD = showString "d"
        sFile FileE = showString "e"
        sFile FileF = showString "f"
        sFile FileG = showString "g"
        sFile FileH = showString "h"
        sRank Rank1 = showString "1"
        sRank Rank2 = showString "2"
        sRank Rank3 = showString "3"
        sRank Rank4 = showString "4"
        sRank Rank5 = showString "5"
        sRank Rank6 = showString "6"
        sRank Rank7 = showString "7"
        sRank Rank8 = showString "8"
        sPromotion PQueen = showString "=Q"
        sPromotion PRook = showString "=R"
        sPromotion PBishop = showString "=B"
        sPromotion PKnight = showString "=N"
        sCheck Check = showString "+"
        sCheck Checkmate = showString "#"
        showEmpty = showString ""

-- |A list of all promotion targets.
allPromotionTargets :: [PromotionTarget]
allPromotionTargets = [PKnight, PBishop, PRook, PQueen]

-- |Gets the appropriate piece for the given promotion target and color.
getPieceForPromotionTarget :: Color -> PromotionTarget -> Piece
getPieceForPromotionTarget color PKnight = (Knight, color)
getPieceForPromotionTarget color PBishop = (Bishop, color)
getPieceForPromotionTarget color PRook = (Rook, color)
getPieceForPromotionTarget color PQueen = (Queen, color)

-- |Gets a move context for the given position and move data.
--  This is the main move validation function, through which all moves pass.
getMoveContext :: PositionContext
                  -> Move
                  -> Maybe PromotionTarget
                  -> Maybe MoveAnnotation
                  -> Either MoveError MoveContext
getMoveContext pc (Move (fromCell, toCell)) mpt mAnnotation = do
    let square = getSquare (position pc) fromCell
    piece <- maybeToEither NoPieceAtSourceSquare square
    let move = createMove fromCell toCell
    unless (pieceCanGetToSquare pc piece move) $ Left PieceCannotReachSquare
    mcd <- getCastlingDataIfNecessary pc piece move
    mPromotionTarget <- getPromotionOrError piece move mpt
    let mc = MkMoveContext {
        mainFromCell = fromCell,
        mainToCell = toCell,
        mainPiece = piece,
        player = currentPlayer pc,
        isCapture = isMoveCapture (position pc) move,
        enPassantCell = getEnPassantCell pc piece move,
        castlingData = mcd,
        promotionTarget = mPromotionTarget,
        moveAnnotation = mAnnotation
    }
    -- This last piece of validation has to be done after the move context has
    -- been created, because we need to create a hypothetical position based on
    -- the move and see if the king is in check.
    -- It would be possible to write wouldMoveLeaveKingInCheck such that it
    -- only requires a PositionContext and a Move, but it isn't really worth
    -- the trouble.
    when (wouldMoveLeaveKingInCheck pc mc) $ Left MoveWouldLeaveKingInCheck
    return mc

-- |Determines whether the given move would leave the king in check. This is
--  done by creating a hypothetical next position based on the move and
--  checking if the king is in check.
wouldMoveLeaveKingInCheck :: PositionContext -> MoveContext -> Bool
wouldMoveLeaveKingInCheck pc mc =
    -- We ignore castling rights, en passant etc.
    -- The only thing we care about here is whether the king would be left in
    -- check by the move.
    let pc' = pc { position = updatePosition (position pc) (getPositionUpdates mc) } in
    isKingInCheck (currentPlayer pc) (position pc')

-- |Gets castling data for the given move. Returns Nothing if no castling is
--  implied, or a MoveError if castling is necesary but not possible, e.g. if
--  the move is white king from e1 to c1 but white does not have the necessary
--  castling rights, or the rook is missing, or the move is blocked, etc.
--
--  See getCastlingDataOrError for details.
getCastlingDataIfNecessary :: PositionContext
                              -> Piece
                              -> Move
                              -> Either MoveError (Maybe CastlingData)
getCastlingDataIfNecessary pc piece move =
    maybe (Right Nothing) (fmap Just . getCastlingDataOrError pc) (getCastling piece move)

-- |Gets castling data or an error if castling is not possible. Checks that the
--  player has the necessary castling rights, and that both the rook and king
--  are able to make the move. This includes checking that none of the cells
--  the king will pass through are under attack.
getCastlingDataOrError :: PositionContext
                          -> Castling
                          -> Either MoveError CastlingData
getCastlingDataOrError pc castling
    | castling `S.notMember` castlingRights pc =
        Left (DoesNotHaveCastlingRights castling)
    | otherwise = maybeToEither (CastlingIsNotPossible castling) $ do
        let (Move (fromCell, toCell)) = rookCastlingMove castling
        guard $ validateKingForCastling pc castling
        guard $ validateRookForCastling pc (createMove fromCell toCell)
        return $ MkCastlingData fromCell toCell castling

-- |Gets a castling specification for the given move, or Nothing if no castling
--  is required.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
getCastling :: Piece -> Move -> Maybe Castling
getCastling (King, color) move =
    findCastlingMove (map withMove (S.toList allCastlingRights)) move color where
        withMove castling = (castling, kingCastlingMove castling)
getCastling _ _ = Nothing

-- |Finds the appropriate castling move based on a list of castling moves.
findCastlingMove :: [(Castling, Move)] -> Move -> Color -> Maybe Castling
findCastlingMove [] _ _ = Nothing
findCastlingMove (((direction, color'), move') : xs) move color =
    if move == move' && color == color'
        then Just (direction, color)
        else findCastlingMove xs move color

-- |Validates that the king is able to make the given castling move.
validateKingForCastling :: PositionContext -> Castling -> Bool
validateKingForCastling pc castling
    | isKingInCheck current (position pc) = False
    | any (hasPiece (position pc)) kingJourneyCells = False
    | any (anyPieceAttacks opponent (position pc)) kingJourneyCells = False
    | otherwise = True where
        current = currentPlayer pc
        opponent = otherColor current
        kingJourneyCells = kingCastlingJourneyCells castling

-- |Validates that the rook is able to make the given castling move.
validateRookForCastling :: PositionContext -> Move -> Bool
validateRookForCastling pc (Move (fromCell, toCell)) =
    maybe False validate (getSquare (position pc) fromCell) where
        move = createMove fromCell toCell
        validate (Rook, col) = pieceCanGetToSquare pc (Rook, col) move
        validate _ = False

-- |Gets the appropriate king move based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
kingCastlingMove :: Castling -> Move
kingCastlingMove castling =
    createMove (kingCastlingSource castling) (kingCastlingDestination castling)

-- |Gets the source cell for the king based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
kingCastlingSource :: Castling -> Cell
kingCastlingSource (_, White) = e1
kingCastlingSource (_, Black) = e8

-- |Gets the destination cell for the king based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
kingCastlingDestination :: Castling -> Cell
kingCastlingDestination (Kingside, White) = g1
kingCastlingDestination (Kingside, Black) = g8
kingCastlingDestination (Queenside, White) = c1
kingCastlingDestination (Queenside, Black) = c8

-- |Gets the list of cells that the king passes through on its way to the
--  castling destination cell. Includes the last cell, but not the first one.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
kingCastlingJourneyCells :: Castling -> [Cell]
kingCastlingJourneyCells (Kingside, White) = [f1, g1]
kingCastlingJourneyCells (Kingside, Black) = [f8, g8]
kingCastlingJourneyCells (Queenside, White) = [d1, c1]
kingCastlingJourneyCells (Queenside, Black) = [d8, c8]

-- |Gets the appropriate rook move based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
rookCastlingMove :: Castling -> Move
rookCastlingMove castling =
    createMove (rookCastlingSource castling) (rookCastlingDestination castling)

-- |Gets the source cell for the rook based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
rookCastlingSource :: Castling -> Cell
rookCastlingSource (Kingside, White) = h1
rookCastlingSource (Kingside, Black) = h8
rookCastlingSource (Queenside, White) = a1
rookCastlingSource (Queenside, Black) = a8

-- |Gets the destination cell for the rook based on a castling specification.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
rookCastlingDestination :: Castling -> Cell
rookCastlingDestination (Kingside, White) = f1
rookCastlingDestination (Kingside, Black) = f8
rookCastlingDestination (Queenside, White) = d1
rookCastlingDestination (Queenside, Black) = d8

-- |Gets the en passant cell based on the given move, or Nothing if no
--  en passant cell is applicable.
--
--  Example: If the piece is a black pawn and the move is c7-c5, this will
--  return c6.
getEnPassantCell :: PositionContext -> Piece -> Move -> Maybe Cell
getEnPassantCell (MkPositionContext { previousEnPassantCell = Just cell }) (Pawn, color) (Move (_, toCell))
    | cell == toCell = Just $ getCapturedPawnCell color cell
    | otherwise = Nothing where
        getCapturedPawnCell White (Cell (file, _)) = Cell (file, Rank5)
        getCapturedPawnCell Black (Cell (file, _)) = Cell (file, Rank4)
getEnPassantCell _ _ _ = Nothing

-- |Gets promotion data for the given piece, move and promotion target.
--
--  Returns an error if no promotion target was given but one was needed, or
--  if no promotion target was needed but one was given anyway. Otherwise it
--  returns the promotion target when the move is actually a promotion, or
--  Nothing if no promotion is needed.
getPromotionOrError :: Piece -> Move -> Maybe PromotionTarget -> Either MoveError (Maybe PromotionTarget)
getPromotionOrError piece move mpt
    | isMovePromotion piece move = Just <$> maybeToEither PromotionIsNeeded mpt
    | isJust mpt = Left PromotionIsNotNeeded
    | otherwise = Right Nothing

-- |Performs the actual piece movement. Updates all the data in the position
--  context based on the data in the move context. The move is assumed to be
--  valid at this point, so all bets are off if it isn't.
performMove :: PositionContext -> MoveContext -> PositionContext
performMove pc mc =
    let move = createMove (mainFromCell mc) (mainToCell mc)
        piece = mainPiece mc in
    MkPositionContext {
        position = updatePosition (position pc) (getPositionUpdates mc),
        currentPlayer = otherColor (currentPlayer pc),
        castlingRights = updateCastlingRights (castlingRights pc) move,
        previousEnPassantCell = updatePreviousEnPassantCell piece move,
        halfMoveClock = updateHalfMoveClock (halfMoveClock pc) mc,
        moveCount = updateMoveCount (moveCount pc) (currentPlayer pc)
    }

-- |Same as performMove except it returns a tuple with the move and the new
--  position rather than just the position.
performMove' :: PositionContext -> MoveContext -> (MoveContext, PositionContext)
performMove' pc mc = (mc, performMove pc mc)

-- |Gets a list of position updates for the given move. Each update is a pair
--  containing a cell, and either Nothing if the cell should be vacated, or the
--  piece that should occupy the cell.
getPositionUpdates :: MoveContext -> [(Cell, Square)]
getPositionUpdates mc =
  let mpt = fmap (getPieceForPromotionTarget (player mc)) (promotionTarget mc) in
  concat [getMainPositionUpdates (mainFromCell mc) (mainToCell mc) (mainPiece mc) mpt,
          getCastlingRookPositionUpdates (castlingData mc),
          getEnPassantUpdates (enPassantCell mc)]

-- |Gets position updates for the main move in a move context.
--
--  The source cell is vacated and the destination cell is occupied by the
--  piece, or by the promotion target if the move is a promotion.
getMainPositionUpdates :: Cell -> Cell -> Piece -> Square -> [(Cell, Square)]
getMainPositionUpdates fromCell toCell piece mpt =
    [(fromCell, Nothing), (toCell, Just $ fromMaybe piece mpt)]

-- |Gets position updates for the rook if the move is a castling move.
--
--  Returns an empty list if this is not a castling move.
getCastlingRookPositionUpdates :: Maybe CastlingData -> [(Cell, Square)]
getCastlingRookPositionUpdates (Just cd) =
    let (_, color) = castlingSpec cd in
    [(castleRookFromCell cd, Nothing), (castleRookToCell cd, Just (Rook, color))]
getCastlingRookPositionUpdates Nothing = []

-- |Gets position updates for an en passant capture.
--
--  Returns an empty list if there's no en passant capture, or a list where the
--  en passant cell is vacated if it actually is an en passant capture. (There
--  can only ever be one update, but the rest of the code is simpler when we
--  return a list.)
getEnPassantUpdates :: Maybe Cell -> [(Cell, Square)]
getEnPassantUpdates = map (flip (,) Nothing) . maybeToList

-- |Updates the castling given set of rights based on the given move. Castling
--  rights are removed if the king or rook has moved.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the positions of the rook and king.
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

-- |Updates the previous en passant cell for the position context.
--
--  Returns the cell on which a pawn can be captured en passant on the next
--  move, or Nothing if en passant is not applicable.
updatePreviousEnPassantCell :: Piece -> Move -> Maybe Cell
updatePreviousEnPassantCell (Pawn, White) (Move (Cell (file, Rank2), Cell (_, Rank4))) =
    Just (createCell file Rank3)
updatePreviousEnPassantCell (Pawn, Black) (Move (Cell (file, Rank7), Cell (_, Rank5))) =
    Just (createCell file Rank6)
updatePreviousEnPassantCell _ _ = Nothing

-- |Updates the half-move clock of the position.
--
--  The count is reset if the move is a pawn move or a capture.
updateHalfMoveClock :: Int -> MoveContext -> Int
updateHalfMoveClock _ (MkMoveContext { isCapture = True }) = 0
updateHalfMoveClock _ (MkMoveContext { mainPiece = (Pawn, _) }) = 0
updateHalfMoveClock n _ = succ n

-- |Updates the full move count of the position.
--
--  The count is increased if it was a black move, and maintained if it was a
--  white move.
updateMoveCount :: Int -> Color -> Int
updateMoveCount n White = n
updateMoveCount n Black = succ n

-- |Determines whether a piece can make a given move in some position.
--
--  This does not account for whether the king is currently in check or will
--  be left in check by this move.
pieceCanGetToSquare :: PositionContext -> Piece -> Move -> Bool
pieceCanGetToSquare pc piece move =
    pieceCanReach pc piece move && hasClearPath (position pc) piece move

-- |Determines whether the given piece can reach the given destination cell
--  from the given source cell in the given position.
--
--  This function basically considers the board to be empty apart from the
--  piece. It doesn't check whether the are other pieces in the way.
pieceCanReach :: PositionContext -> Piece -> Move -> Bool
pieceCanReach pc (Pawn, color) move =
    pawnCanReach color (position pc) move || isValidEnPassantCapture pc move
pieceCanReach pc (King, color) move = kingCanReach color pc move
pieceCanReach _ (chessman, _) move = chessmanCanReach chessman move

-- |Determines whether the given piece attacks the given destination cell from
--  the given source cell.
--
--  There's a slight inaccuracy here, in that en passant isn't considered. This
--  is because the function takes a 'Position' and not a 'PositionContext'.
--  Pawns will therefore not be considered to attack cells where they are in
--  reality able to capture en passant. Because this function is currently only
--  used internally to determine if the king is in check, and en passant does
--  not apply in such cases, this does not result in any bugs, but it must be
--  considered if a public function is made at some point. Such a function
--  needs to take a 'PositionContext' as input instead.
pieceAttacks :: Position -> Piece -> Move -> Bool
pieceAttacks pos (Pawn, color) move = isValidPawnCapture color pos move
pieceAttacks _ (King, _) move = kingCanReachWithoutCastling move
pieceAttacks pos (chessman, color) move = chessmanCanReach chessman move && hasClearPath pos (chessman, color) move

-- |Determines whether the given chessman can reach the given square.
--
--  Note! Must not be used with pawns or kings. For pawns and kings we need to
--  know the color in addition to the chessman to determine if they can reach
--  the square in question, because of double moves and castling respectively.
--  The functions pawnCanReach and kingCanReach should be used instead for
--  those chessmen.
chessmanCanReach :: Chessman -> Move -> Bool
chessmanCanReach Bishop = bishopCanReach
chessmanCanReach Knight = knightCanReach
chessmanCanReach Rook = rookCanReach
chessmanCanReach Queen = queenCanReach
chessmanCanReach Pawn = error "Use pawnCanReach instead for pawns"
chessmanCanReach King = error "Use kingCanReach instead for kings"

-- |Determines whether the given piece has a clear path for the given move in
--  the given position.
--
--  For this to return true, the destination cell must not be occupied by a
--  friendly piece, and none of the intermediate cells must be occupied by any
--  piece. Note that the notion of intermediate cells doesn't apply to knights.
hasClearPath :: Position -> Piece -> Move -> Bool
hasClearPath pos (chessman, color) (Move (fromCell, toCell))
    | hasPieceOfColor color pos toCell = False
    | chessman == Knight = True -- Nothing stands in the way of a knight.
    | otherwise = not $ any (hasPiece pos) (interpolateCells fromCell toCell)

-- |Creates a list of cells that are strictly between the two given cells.
--
--  Note! This function will give unhelpful results if given cells that aren't
--  on the same straight line or diagonal. For example, if given a1 and c5,
--  this will return [b2]. We trust that other parts of the move validation
--  logic have previously checked that the piece can actually get to the cell
--  in question, so these bogus results should not affect the correctness of
--  the algorithm.
interpolateCells :: Cell -> Cell -> [Cell]
interpolateCells (Cell (fromFile, fromRank)) (Cell (toFile, toRank))
    | fromFile == toFile = map (createCell fromFile) ranks
    | fromRank == toRank = map (`createCell` fromRank) files
    | otherwise = zipWith createCell files ranks
    where
        files = interpolate fromFile toFile
        ranks = interpolate fromRank toRank

-- |Determines whether a pawn can make some move in some position.
--
--  For this to be true, the move must be a regular pawn move (one cell ahead
--  vertically), a double pawn move (two cells ahead vertically when starting
--  on the second or seventh rank, depending on the color), or a capture (one
--  cell ahead vertically and one rank to either side).
pawnCanReach :: Color -> Position -> Move -> Bool
pawnCanReach color pos move =
    isValidRegularPawnMove color pos move ||
    isValidDoublePawnMove color pos move ||
    isValidPawnCapture color pos move

-- |Determines whether the given move is valid as a regular pawn move.
--
--  The pawn must travel one cell ahead vertically.
isValidRegularPawnMove :: Color -> Position -> Move -> Bool
isValidRegularPawnMove color pos (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    -- If there's somehow a pawn on the first/last rank we immediately return
    -- False to avoid errors from pred/succ.
    fromRank `notElem` [Rank1, Rank8] &&
    fromFile == toFile &&
    toRank == expectedRank color fromRank &&
    not (hasPiece pos (createCell toFile toRank))
    where
        expectedRank White = succ
        expectedRank Black = pred

-- |Determines whether the given move is valid as a double pawn move.
--
--  The pawn must travel two cells ahead vertically when starting from the
--  second rank if white, or the seventh rank if black.
isValidDoublePawnMove :: Color -> Position -> Move -> Bool
isValidDoublePawnMove color pos (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile &&
    checkRanks color fromRank toRank &&
    not (hasPiece pos toCell) &&
    hasClearPath pos (Pawn, color) (createMove fromCell toCell)
    where
        fromCell = createCell fromFile fromRank
        toCell = createCell toFile toRank
        checkRanks White Rank2 Rank4 = True
        checkRanks Black Rank7 Rank5 = True
        checkRanks _ _ _  = False

-- |Determines whether the given move is valid as a pawn capture.
--
--  The pawn must travel one cell ahead vertically and move one file to either
--  side, and the destination cell must contain a piece of the opposite color.
isValidPawnCapture :: Color -> Position -> Move -> Bool
isValidPawnCapture color pos (Move (fromCell, toCell)) =
    isValidPawnCaptureMove color (createMove fromCell toCell) &&
    hasPieceOfColor (otherColor color) pos toCell

-- |Determines whether the given move is valid as an en passant pawn capture.
--
--  The pawn must travel one cell ahead vertically and move one file to either
--  side, and the destination cell must be the previous en passant cell for the
--  position.
isValidEnPassantCapture :: PositionContext -> Move -> Bool
isValidEnPassantCapture pc (Move (fromCell, toCell)) =
    previousEnPassantCell pc == Just toCell &&
    isValidPawnCaptureMove (currentPlayer pc) (createMove fromCell toCell)

-- |Determines whether the given move is valid as a pawn capture, without
--  considering whether there's an enemy piece at the destination square.
--
--  The pawn must travel one cell ahead vertically and move one file to either
--  side.
isValidPawnCaptureMove :: Color -> Move -> Bool
isValidPawnCaptureMove color (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    -- If there's somehow a pawn on the first/last rank we immediately return
    -- False to avoid errors from pred/succ.
    fromRank `notElem` [Rank1, Rank8] &&
    toFile `elem` adjacentFiles fromFile &&
    toRank == expectedRank color fromRank
    where
        expectedRank White = succ
        expectedRank Black = pred
        adjacentFiles FileA = [FileB]
        adjacentFiles FileH = [FileG]
        adjacentFiles file = [pred file, succ file]

-- |Determines whether a king can make the given move, including by castling.
kingCanReach :: Color -> PositionContext -> Move -> Bool
kingCanReach color pc move =
    let checkCastling = getCastling (King, color) move in
    maybe (kingCanReachWithoutCastling move) (kingCanReachByCastling pc) checkCastling

-- |Determines whether a king can make the given castling move.
--
--  This is equivalent to getCastlingDataOrError but returning True / False
--  instead of a Maybe MoveError.
kingCanReachByCastling :: PositionContext -> Castling -> Bool
kingCanReachByCastling pc castling = isRight $ getCastlingDataOrError pc castling

-- |Determines whether a king can make the given non-castling move.
kingCanReachWithoutCastling :: Move -> Bool
kingCanReachWithoutCastling (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    abs (fromFileN - toFileN) <= 1 && abs (fromRankN - toRankN) <= 1 where
        fromFileN = fromEnum fromFile
        fromRankN = fromEnum fromRank
        toFileN = fromEnum toFile
        toRankN = fromEnum toRank
        

-- |Determines whether a bishop can make the given move.
bishopCanReach :: Move -> Bool
bishopCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    abs (fromFileN - toFileN) == abs (fromRankN - toRankN) where
        fromFileN = fromEnum fromFile
        fromRankN = fromEnum fromRank
        toFileN = fromEnum toFile
        toRankN = fromEnum toRank

-- |Determines whether a knight can make the given move.
knightCanReach :: Move -> Bool
knightCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    (abs (fromFileN - toFileN) == 1 && abs (fromRankN - toRankN) == 2) ||
    (abs (fromFileN - toFileN) == 2 && abs (fromRankN - toRankN) == 1)
    where
        fromFileN = fromEnum fromFile
        fromRankN = fromEnum fromRank
        toFileN = fromEnum toFile
        toRankN = fromEnum toRank

-- |Determines whether a rook can make the given move.
rookCanReach :: Move -> Bool
rookCanReach (Move (Cell (fromFile, fromRank), Cell (toFile, toRank))) =
    fromFile == toFile || fromRank == toRank

-- |Determines whether a queen can make the given move.
queenCanReach :: Move -> Bool
queenCanReach move = rookCanReach move || bishopCanReach move

-- |Determines whether the king is in check in the given position.
isKingInCheck :: Color -> Position -> Bool
isKingInCheck color pos =
    anyPieceAttacks (otherColor color) pos (findKing color pos)

-- |Determines whether the king is checkmate in the given position.
--
--  That means that the current player must have no moves, and that the king
--  must be in check.
isCheckmate :: PositionContext -> Bool
isCheckmate pc =
    hasNoMoves pc && isKingInCheck (currentPlayer pc) (position pc)

-- |Determines whether the king is stalemate in the given position.
--
--  That means that the current player must have no moves, and that the king
--  must *not* be in check.
isStalemate :: PositionContext -> Bool
isStalemate pc =
    hasNoMoves pc && not (isKingInCheck (currentPlayer pc) (position pc))

-- |Determines whether the current player has no moves.
hasNoMoves :: PositionContext -> Bool
hasNoMoves = null . findAllLegalMoves

-- |Gets a check state for the given position.
--
--  Returns 'Checkmate' if the king is checkmated, 'Check' if the king is in
--  check but there are available moves, and otherwise Nothing.
getCheckState :: PositionContext -> Maybe CheckState
getCheckState pc
    | isCheck && hasNoMoves pc = Just Checkmate
    | isCheck = Just Check
    | otherwise = Nothing where
        isCheck = isKingInCheck (currentPlayer pc) (position pc)

-- |Finds the king of the given position on the board.
--  Note! This will give an error if there's no king on the board.
findKing :: Color -> Position -> Cell
findKing color pos = fst . head $ filter isKing (piecesOfColor color pos) where
    -- TODO: It might be a good idea to cache this information somewhere,
    -- since quite a bit of time is spent looking for the king.
    -- Should also change the function so it isn't partial.
    isKing (_, (King, _)) = True
    isKing _ = False

-- |Determines whether any enemy piece attacks the given cell.
anyPieceAttacks :: Color -> Position -> Cell -> Bool
anyPieceAttacks color pos targetCell =
    let pieces = piecesOfColor color pos
        f (cell, piece) = (piece, createMove cell targetCell)
        attackingMoves = map f pieces in
    any (uncurry (pieceAttacks pos)) attackingMoves

-- |Determines whether the given move is a piece capture.
isMoveCapture :: Position -> Move -> Bool
isMoveCapture pos (Move (_, toCell)) = hasPiece pos toCell

-- |Determines whether the given move is a promotion.
isMovePromotion :: Piece -> Move -> Bool
isMovePromotion (Pawn, White) (Move (_, Cell (_, Rank8))) = True
isMovePromotion (Pawn, Black) (Move (_, Cell (_, Rank1))) = True
isMovePromotion _ _ = False

-- |Attempts to find a source cell for an underspecified move.
--  See disambiguateSourceCell for details.
--
--  This function is Chess960 incompatible, because it makes assumptions about
--  the position of the king when castling.
getSourceCell :: PositionContext -> UnderspecifiedMove -> Either MoveError Cell
getSourceCell (MkPositionContext { currentPlayer = White }) (CastleMove _) = return e1
getSourceCell (MkPositionContext { currentPlayer = Black }) (CastleMove _) = return e8
getSourceCell pc unspecMove =
    case createCell <$> knownFromFile unspecMove <*> knownFromRank unspecMove of
        Just c -> return c -- No ambiguity.
        Nothing -> disambiguateSourceCell pc unspecMove

-- |Attempts to disambiguate the source cell for an underspecified move in
--  cases either the source file or the source rank has been omitted.
--
--  If there are multiple pieces of the same type that can reach the
--  destination cell, the disambiguation fields on the underspecified move are
--  consulted. If no pieces can make the move or multiple pieces can make the
--  move, the move is invalid.
disambiguateSourceCell :: PositionContext
                          -> UnderspecifiedMove
                          -> Either MoveError Cell
disambiguateSourceCell pc unspecMove =
    let toCell = knownToCell unspecMove
        piece = (knownChessman unspecMove, currentPlayer pc)
        mFromFile = knownFromFile unspecMove
        mFromRank = knownFromRank unspecMove in
    case findCandidateSourceCells pc toCell piece mFromFile mFromRank of
        [] -> Left NoPieceAtSourceSquare
        [single] -> Right single
        multiple -> Left $ InsufficientDisambiguation multiple

-- |Finds a list of candidate source cells for an underspecified move.
--
--  If the source file is given, only pieces on that file will be considered.
--
--  If the source rank is given, only pieces on that rank will be considered.
findCandidateSourceCells :: PositionContext
                            -> Cell
                            -> Piece
                            -> Maybe File
                            -> Maybe Rank
                            -> [Cell]
findCandidateSourceCells pc toCell piece mFromFile mFromRank = compress $ do
    mc <- findAllLegalMoves pc
    guard $ toCell == mainToCell mc
    let fromCell = mainFromCell mc
        Cell (f, r) = fromCell
    guard $ maybe True (== f) mFromFile
    guard $ maybe True (== r) mFromRank
    guard $ hasPieceOfType piece (position pc) fromCell
    return fromCell

-- |Creates a move based on a source cell and a destination cell.
createMove :: Cell -> Cell -> Move
createMove fromCell toCell = Move (fromCell, toCell)

-- |Creates a minimally specified move based on the given move and position.
--
--  The source file and rank will only be set if ambiguity necessitates it.
--
--  This function gives a MoveError (specifically `PieceCannotReachSquare`),
--  if the game is invalid.
createMinimalMove :: PositionContext -> MoveContext -> Either MoveError MinimalMove
createMinimalMove _ (MkMoveContext { castlingData = Just (MkCastlingData { castlingSpec = (direction, _) }), moveAnnotation = mAnnotation }) =
    Right (CastleMove direction, mAnnotation)
createMinimalMove pc mc =
    -- Find candidates when we only specify the destination and the piece.
    -- If there's only a single result we don't need any disambiguation.
    -- If we get multiple results we have to disambiguate by file and/or rank.
    let fromCell = mainFromCell mc
        toCell = mainToCell mc
        piece = mainPiece mc
        (chessman, _) = piece
        isCap = isCapture mc
        mpt = promotionTarget mc
        mCheckState = getCheckState pc
        mAnnotation = moveAnnotation mc in
    -- In order:
    -- An empty list indicates that the move is not valid for the position. Should only happen if the game is invalid.
    -- A singleton list indicates that only one piece can reach the square. Disambiguation is only needed if the move is a pawn capture.
    -- Multiple pieces can reach the square. File and/or rank disambiguation is needed.
    case findCandidateSourceCells pc toCell piece Nothing Nothing of
        [] -> Left PieceCannotReachSquare
        [_] -> Right $ resolveSimpleDisambiguation fromCell toCell chessman isCap mpt mCheckState mAnnotation
        multiple -> Right $ resolveDisambiguation fromCell toCell chessman isCap mpt mCheckState mAnnotation multiple

-- |Resolves disambiguation when we only have one candidate source cell.
--
--  We disambiguate by file iff the piece is a pawn and the move is a capture,
--  and never disambiguate by rank.
resolveSimpleDisambiguation :: Cell
                               -> Cell
                               -> Chessman
                               -> Bool
                               -> Maybe PromotionTarget
                               -> Maybe CheckState
                               -> Maybe MoveAnnotation
                               -> MinimalMove
resolveSimpleDisambiguation (Cell (fromFile, _)) toCell chessman isCap mpt mCheckState mAnnotation =
    let mFromFile = if chessman == Pawn && isCap then Just fromFile else Nothing in
    (MkUnderspecifiedMove toCell chessman mFromFile Nothing isCap mpt mCheckState, mAnnotation)

-- |Resolves disambiguation when we have multiple candidate source cells.
--
--  We disambiguate by file if there are multiple candidates on the same rank,
--  and by rank if there are multiple candidates on the same file. In some rare
--  cases it's even necessary to disambiguate by both at the same time.
resolveDisambiguation :: Cell
                         -> Cell
                         -> Chessman
                         -> Bool
                         -> Maybe PromotionTarget
                         -> Maybe CheckState
                         -> Maybe MoveAnnotation
                         -> [Cell]
                         -> MinimalMove
resolveDisambiguation (Cell (actualFile, actualRank)) toCell chessman isCap mpt mCheckState mAnnotation possibleCells =
    -- Given an actual move, figure out how much we need to disambiguate based on a given list of possible source cells.
    let candidateCells = L.delete (Cell (actualFile, actualRank)) possibleCells
        hasOnFile = hasOtherOnFile candidateCells
        hasOnRank = hasOtherOnRank candidateCells
        fileIfNecessary = if hasOnRank || (not hasOnFile && not hasOnRank)
                              then Just actualFile
                              else Nothing
        rankIfNecessary = if hasOnFile
                              then Just actualRank
                              else Nothing in
    (MkUnderspecifiedMove toCell chessman fileIfNecessary rankIfNecessary isCap mpt mCheckState, mAnnotation)
    where
        hasOtherOnFile = any (\(Cell (file, _)) -> file == actualFile)
        hasOtherOnRank = any (\(Cell (_, rank)) -> rank == actualRank)

-- |Gets a list of all possible promotion targets if the move is a promotion,
--  or a list containing a single Nothing item if it isn't.
getPossiblePromotionTargets :: Piece -> Move -> [Maybe PromotionTarget]
getPossiblePromotionTargets piece move
    | isMovePromotion piece move = map Just allPromotionTargets
    | otherwise = [Nothing]

-- |Gets a list containing all legal moves for a single piece located at a
--  given cell.
findLegalMoves :: PositionContext -> (Cell, Piece) -> [MoveContext]
findLegalMoves pc (fromCell, piece) = do
    toCell <- allCells
    pt <- getPossiblePromotionTargets piece (createMove fromCell toCell)
    rights [getMoveContext pc (createMove fromCell toCell) pt Nothing]

-- |Gets a list of all legal moves for the current player in some position.
findAllLegalMoves :: PositionContext -> [MoveContext]
findAllLegalMoves pc = piecesOfColor (currentPlayer pc) (position pc) >>= findLegalMoves pc

-- |Determines whether the move identified by the given coordinates and
--  promotion target is valid.
--
--  Note! The type of promotion target will never affect the legality of the
--  move. If you don't know what promotion target to use then any of the four
--  alternatives will do.
isLegalMove :: PositionContext -> Move -> Maybe PromotionTarget -> Bool
isLegalMove pc move mpt = isRight $ getMoveContext pc move mpt Nothing

-- |Determines whether the given minimal move is valid.
--
--  This is shorthand for checking that makeMinimalMove doesn't give an error.
isLegalMinimalMove :: PositionContext -> MinimalMove -> Bool
isLegalMinimalMove pc miniMove = isRight $ makeMinimalMove pc miniMove

-- |Determines whether the move identified by the given coordinates is valid.
--
--  The move must not be a promotion.
isLegalNonPromotionMove :: PositionContext -> Move -> Bool
isLegalNonPromotionMove pc move = isLegalMove pc move Nothing

-- |Determines whether the move identified by the given coordinates is valid.
--
--  The move must be a promotion.
isLegalPromotion :: PositionContext -> Move -> PromotionTarget -> Bool
isLegalPromotion pc move pt = isLegalMove pc move (Just pt)

-- |Makes a move with the given move data.
--
--  If the move is valid, a MoveContext and new PositionContext are returned.
--
--  If the move is invalid, an describing why the move was rejected is returned.
makeMove :: PositionContext
            -> Move
            -> Maybe PromotionTarget
            -> Maybe MoveAnnotation
            -> Either MoveError (MoveContext, PositionContext)
makeMove pc move mpt mAnnotation =
    performMove' pc <$> getMoveContext pc move mpt mAnnotation

-- |Makes a move that's guaranteed not to be a promotion.
--
--  If the move is valid, a MoveContext and new PositionContext are returned.
--
--  If the move is invalid, an describing why the move was rejected is returned.
makeNonPromotionMove :: PositionContext
                        -> Move
                        -> Either MoveError (MoveContext, PositionContext)
makeNonPromotionMove pc move = makeMove pc move Nothing Nothing

-- |Makes a move that's guaranteed to be a promotion.
--
--  If the move is valid, a MoveContext and new PositionContext are returned.
--
--  If the move is invalid, an describing why the move was rejected is returned.
makePromotion :: PositionContext
                 -> Move
                 -> PromotionTarget
                 -> Either MoveError (MoveContext, PositionContext)
makePromotion pc move pt = makeMove pc move (Just pt) Nothing

-- |Makes an underspecified move.
--
--  This is appropriate in situations where the source of the move is a
--  notation type like SAN, where some information is omitted when it's
--  redundant.
--
--  In addition to all the regular MoveError types, this may additionally
--  return the 'InsufficientDisambiguation' error type, indicating that the
--  move is valid but there are multiple candidates pieces that can make it,
--  and it's impossible to tell which one to use.
makeMinimalMove :: PositionContext
                   -> MinimalMove
                   -> Either MoveError (MoveContext, PositionContext)
makeMinimalMove pc (CastleMove direction, mAnnotation) =
    let move = kingCastlingMove (direction, currentPlayer pc) in
    makeMove pc move Nothing mAnnotation
makeMinimalMove pc (unspecMove, mAnnotation) = do
    fromCell <- getSourceCell pc unspecMove
    let move = createMove fromCell (knownToCell unspecMove)
    makeMove pc move (knownPromotionTarget unspecMove) mAnnotation

-- |A QuickCheck generator for a random move.
--
--  This is faster than generating a random move with 'findAllLegalMoves' and
--  then playing it with 'makeMove' because this function only needs to
--  validate the move once. However, it still needs to generate every legal
--  move in order for the randomness to be fair.
--
--  Note that if there are no legal moves in the position, the generator will
--  always return a Nothing move and the same position that it started with.
randomMove :: PositionContext -> Gen (Maybe MoveContext, PositionContext)
randomMove pc =
    case findAllLegalMoves pc of
        [] -> return (Nothing, pc)
        moves -> first Just . performMove' pc <$> elements moves
