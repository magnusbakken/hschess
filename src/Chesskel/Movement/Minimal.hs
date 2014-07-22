{-|
Module      : Chesskel.Board
Description : Functions for minimally specified moves.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions related to minimally specified moves. In
Standard Algebraic Notation you only specify the source file and rank for a
move when it's necessary to avoid ambiguity. This module lets you use moves
specified in that manner with the Chesskel API.
-} 
module Chesskel.Movement.Minimal (
    UnderspecifiedMove (..),
    MinimalMove,
    makeMinimalMove,
    isLegalMinimalMove,
    createMinimalMove
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Utils
import Control.Applicative
import Control.Monad
import qualified Data.List as L

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


-- |Makes an underspecified move in a given position.
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

-- |Determines whether the given minimal move is valid.
--
--  This is shorthand for checking that 'makeMinimalMove' doesn't give an error.
isLegalMinimalMove :: PositionContext -> MinimalMove -> Bool
isLegalMinimalMove pc miniMove = isRight $ makeMinimalMove pc miniMove
