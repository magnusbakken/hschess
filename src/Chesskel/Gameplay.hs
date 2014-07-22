{-|
Module      : Chesskel.Board
Description : Functions for entire chess games.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions related to complete chess games. This includes
both the move and position history of the game, and also metadata such as the
names of the players. If you only need to consider individual positions and
moves you can concentrate on the "Chesskel.Movement" module instead.
-}
module Chesskel.Gameplay (
    GameContext (..),
    HeaderData (..),
    AllHeaderData,
    ExtraHeader (..),
    Result (..),
    
    -- ** Starting games
    -- |Functions that start new games.
    
    startStandardGame,
    startGame,
    
    -- ** Playing moves
    -- |Functions that alter the current position of games by playing moves.
    
    playMove,
    playMinimalMove,
    playNonPromotionMove,
    playPromotion,
    
    -- ** Result changes
    -- |Functions that alter the results of games.
    
    resign,
    makeDraw,
    setResult,
    
    -- ** Headers
    -- |Functions related to game headers.
    
    unknownHeaderData,
    allHeaderData,
    
    -- ** Other
    -- |Functions that don't fit into any of the above categories.
    
    createMinimalMoves,
    randomMoveForGame
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Movement.Minimal
import Control.Monad
import Test.QuickCheck.Gen

-- |A result represents the outcome of a game.
--
--  All games must have a result. If the result is unknown or if the game isn't
--  finished, 'Ongoing' should be used.
data Result = WhiteWin | Draw | BlackWin | Ongoing deriving (Eq)

-- |This set of headers is sometimes known as the Seven Tag Roster, or SRT.
--  These headers are considered obligatory for all games.
data HeaderData = MkHeaderData {
    -- |The tournament, match or league in which the game was played.
    eventHeader :: String,
    
    -- |The physical location or server where the game was played.
    --  When this is a physical location it should preferably be on the format
    --  City, Region COUNTRY.
    siteHeader :: String,
    
    -- |The date the game was played. Should preferably be on the format
    --  YYYY.MM.DD.
    dateHeader :: String,
    
    -- |The playing round within the event. Should preferably be an integer.
    roundHeader :: String,
    
    -- |The name of the white player. Should preferably be on the format
    --  Last Name, First Name.
    whiteHeader :: String,
    
    -- |The name of the black player. Should preferably be on the format
    --  Last Name, First Name.
    blackHeader :: String,
    
    -- |The result of the game. If the result is unknown, 'Ongoing' should be
    --  used.
    resultHeader :: Result
} deriving (Eq)

-- |An extra header (tag) for a game is the combination of a name and a value.
data ExtraHeader = MkExtraHeader {
    -- |The name of the header.
    headerName :: String,
    
    -- |The value of the header. Should never contains quotation marks (\"),
    --  even if they're escaped with backslashes. Values containing quotation
    --  marks will cause PGN files generated from the header to be invalid.
    headerValue :: String
} deriving (Eq)

-- |Header data consisting of the Seven Tag Roster and an optional list of
--  extra headers.
type AllHeaderData = (HeaderData, [ExtraHeader])

-- |A game context encapsulates an entire chess game. It contains all the
--  positions as well as all the moves from the game, and also game headers
--  and the final result.
--
--  You should never modify this object directly. Always use the 'playMove'
--  function(s) to ensure the game remains in a valid state.
data GameContext = MkGameContext {
    -- |The current position. Should always be the same as the last item in the
    --  'positions' list.
    currentPosition :: PositionContext,
    
    -- |The list of positions in the game. Should never be empty.
    --  The first position in the list should be the starting position,
    --  and the last position should be the same as 'currentPosition'.
    positions :: [PositionContext],
    
    -- |The list of moves in the game. The positions and moves should be
    --  interleaved, in the sense that the first position comes before the
    --  first move, then comes the first move, then the second position, etc.
    --  The last move should always lead to the last position, except when
    --  there are no moves at all. The 'positions' list should therefore always
    --  be exactly one item longer than the list of moves.
    moves :: [MoveContext],
    
    -- |The main header data (Seven Tag Roster) for the game.
    mainHeaderData :: HeaderData,
    
    -- |A dynamic list of arbitrary and optional headers.
    extraHeaderData :: [ExtraHeader],
    
    -- |The result of the game. Should only be changed using the 'resign',
    --  'makeDraw' and 'setResult' functions. Doing so will ensure that the
    --  result in the header data is kept in sync.
    gameResult :: Result
} deriving (Eq)

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
    show (MkExtraHeader { headerName = name, headerValue = value }) =
        "[" ++ name ++ " \"" ++ value ++ "\"]"

-- |Gets the result that corresponds to the given color winning.
getWin :: Color -> Result
getWin White = WhiteWin
getWin Black = BlackWin

-- |Used when starting a standard game. All the headers in the SRT will be set
--  to a value indicating that they're unknown, and the result will be set as
--  'Ongoing'.
unknownHeaderData :: HeaderData
unknownHeaderData = MkHeaderData {
    eventHeader = "??",
    siteHeader =  "??",
    dateHeader =  "??",
    roundHeader = "??",
    whiteHeader = "??",
    blackHeader = "??",
    resultHeader = Ongoing
}

-- |Sets the result header in the given HeaderData object.
setResultHeader :: Result -> HeaderData -> HeaderData
setResultHeader result hd = hd { resultHeader = result }

-- |Gets all the header data from the given game.
allHeaderData :: GameContext -> AllHeaderData
allHeaderData gc = (mainHeaderData gc, extraHeaderData gc)

-- |Starts a standard game from the standard chess starting position,
--  using 'unknownHeaderData' for the headers.
startStandardGame :: GameContext
startStandardGame = startGame startPosition (unknownHeaderData, [])

-- |Starts a game from a custom position, with custom header data.
startGame :: PositionContext -> AllHeaderData -> GameContext
startGame pc (hd, extra) = MkGameContext {
    currentPosition = pc,
    positions = [pc],
    moves = [],
    mainHeaderData = hd,
    extraHeaderData = extra,
    gameResult = Ongoing
}

-- |A generic move function used both by playMove and playMinimalMove
playMove' :: (GameContext -> Either MoveError (MoveContext, PositionContext))
             -> GameContext
             -> Either MoveError GameContext
playMove' moveFunc gc = do
    unless (gameResult gc == Ongoing) $ Left GameIsFinished
    (mc, pc') <- moveFunc gc
    return $ updateGameContext mc pc' gc

-- |Plays a single move, with an optional promotion target (only applicable
--  when moving a pawn to a promotion square) and an optional move annotation.
--
--  This may fail, in which case a MoveError is returned. Call 'isLegalMove'
--  first if you want to ensure that the move is legal. However, attempting to
--  play the move and handling the error is easier, since in the event of a
--  move error you will have some idea why the move was not allowed based on
--  the specific MoveError.
--
--  See 'Chesskel.Movement.makeMove' for more information.
playMove :: Move
            -> Maybe PromotionTarget
            -> Maybe MoveAnnotation
            -> GameContext
            -> Either MoveError GameContext
playMove move mpt mAnnotation =
    playMove' (\gc -> makeMove (currentPosition gc) move mpt mAnnotation)

-- |Plays an underspecified move. This allows you to get the functionality from
--  e.g. PGN files where a move is specified as simply e4, without indicating
--  the source square. At least a chessman and a destination square must be
--  specified; the player is inferred from the current position of the game.
--
--  If there are multiple pieces that could've made the move, an
--  'InsufficientDisambiguation' will be returned, containing a list of
--  available source cells.
--
--  See 'Chesskel.Movement.Minimal.makeMinimalMove' for more information.
playMinimalMove :: MinimalMove -> GameContext -> Either MoveError GameContext
playMinimalMove miniMove =
    playMove' (\gc -> makeMinimalMove (currentPosition gc) miniMove)

-- |Shorthand for moves that are guaranteed not to require a promotion target.
--
--  If the move requires a promotion target after all, a 'PromotionIsNeeded'
--  error will be returned.
playNonPromotionMove :: Move -> GameContext -> Either MoveError GameContext
playNonPromotionMove move = playMove move Nothing Nothing

-- |Shorthand for moves that are guaranteed to require a promotion target.
--
--  If the move doesn't require a promotion target after all, a
--  'PromotionIsNotNeeded' error will be returned.
playPromotion :: Move
                 -> PromotionTarget
                 -> GameContext
                 -> Either MoveError GameContext
playPromotion move pt = playMove move (Just pt) Nothing

-- |Makes the player of the given color resign. Equivalent to setting the
--  result to 'WhiteWin' if the given color is 'Black', or 'BlackWin' if the
--  given color is 'White'.
resign :: Color -> GameContext -> GameContext
resign White = setResult BlackWin
resign Black = setResult WhiteWin

-- |Makes the result of the game a draw.
makeDraw :: GameContext -> GameContext
makeDraw = setResult Draw

-- |Sets the given result for the game. This allows you to set a game that was
--  previously marked as finished back to 'Ongoing', or turn it from a 'Draw'
--  into a win or vice versa.
--
--  When possible you should use 'resign' and 'makeDraw' instead, as the intent
--  will be clearer.
setResult :: Result -> GameContext -> GameContext
setResult result gc = gc {
    mainHeaderData = setResultHeader result (mainHeaderData gc),
    gameResult = result
}

-- |Creates a list of minimally specified moves, each of which has the minimum
--  amount of disambiguation needed, for the given game.
--
--  This may return a MoveError because the game could potentially be in an
--  invalid state.
--
--  Refer to 'Chesskel.Movement.createMinimalMove' for more details.
createMinimalMoves :: GameContext -> Either MoveError [MinimalMove]
createMinimalMoves gc = zipWithM createMinimalMove (positions gc) (moves gc)

-- |Updates the given game with a new move, and the new position that will
--  arise after the move has been played. Also updates the result if the
--  position is a checkmate or stalemate.
updateGameContext :: MoveContext
                     -> PositionContext
                     -> GameContext
                     -> GameContext
updateGameContext mc pc gc =
    setResult (getNewResult pc) $ gc {
        currentPosition = pc,
        positions = positions gc ++ [pc],
        moves = moves gc ++ [mc]
    }

-- |Gets a result for the given position.
--
--  Returns 'WhiteWin' or 'BlackWin' if the other side is checkmated,
--  'Draw' if the position is a stalemate, and 'Ongoing' otherwise.
getNewResult :: PositionContext -> Result
getNewResult pc
    | isCheckmate pc = getWin (otherColor (currentPlayer pc))
    | isStalemate pc = Draw
    | otherwise = Ongoing

-- |A QuickCheck generator for playing a random move in a given game.
-- 
--  Uses the `randomMove` generator in "Chesskel.Movement".
randomMoveForGame :: GameContext -> Gen GameContext
randomMoveForGame gc = do
    (mmc, pc) <- randomMove (currentPosition gc)
    case mmc of
        Nothing -> return gc
        Just mc -> return (updateGameContext mc pc gc)
