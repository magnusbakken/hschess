module Chesskel.Gameplay (
    GameContext (currentPosition, positions, moves, mainHeaderData, extraHeaderData, gameResult),
    HeaderData (..),
    AllHeaderData,
    ExtraHeader (..),
    Result (..),
    unknownHeaderData,
    allHeaderData,
    startStandardGame,
    startGame,
    playMove,
    playUnderspecifiedMove,
    playNonPromotionMove,
    playPromotion,
    resign,
    makeDraw,
    setResult,
    createMinimallySpecifiedMoves
) where

import Chesskel.Board
import Chesskel.Movement
import Control.Monad

data Result = WhiteWin | Draw | BlackWin | Ongoing deriving (Eq)

-- This is sometimes known as the "Seven Tag Roster", or SRT.
-- These headers are considered obligatory for all games.
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
type AllHeaderData = (HeaderData, [ExtraHeader])

data GameContext = GC {
    currentPosition :: PositionContext,
    positions :: [PositionContext],
    moves :: [MoveContext],
    mainHeaderData :: HeaderData,
    extraHeaderData :: [ExtraHeader],
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
    show (EH { headerName = name, headerValue = value }) = "[" ++ name ++ " \"" ++ value ++ "\"]"

getWin :: Color -> Result
getWin White = WhiteWin
getWin Black = BlackWin

unknownHeaderData :: HeaderData
unknownHeaderData = HD {
    eventHeader = "?",
    siteHeader = "?",
    dateHeader = "?",
    roundHeader = "?",
    whiteHeader = "?",
    blackHeader = "?",
    resultHeader = Ongoing
}

allHeaderData :: GameContext -> AllHeaderData
allHeaderData gc = (mainHeaderData gc, extraHeaderData gc)

startStandardGame :: GameContext
startStandardGame = startGame startPosition (unknownHeaderData, [])

startGame :: PositionContext -> AllHeaderData -> GameContext
startGame pc (hd, extra) = GC {
    currentPosition = pc,
    positions = [pc],
    moves = [],
    mainHeaderData = hd,
    extraHeaderData = extra,
    gameResult = Ongoing
}

playMove :: Move -> Maybe PromotionTarget -> Maybe MoveAnnotation -> GameContext -> Either MoveError GameContext
playMove move mpt mAnnotation gc = do
    failIfGameIsFinished gc
    (mc, pc') <- makeMove (currentPosition gc) move mpt mAnnotation
    return $ updateGameContext mc pc' gc

failIfGameIsFinished :: GameContext -> Either MoveError ()
failIfGameIsFinished gc = if gameResult gc == Ongoing then Right () else Left GameIsFinished

playUnderspecifiedMove :: UnderspecifiedMove -> GameContext -> Either MoveError GameContext
playUnderspecifiedMove unspecMove gc = do
    (mc, pc') <- makeUnderspecifiedMove (currentPosition gc) unspecMove
    return $ updateGameContext mc pc' gc

playNonPromotionMove :: Move -> GameContext -> Either MoveError GameContext
playNonPromotionMove move = playMove move Nothing Nothing

playPromotion :: Move -> PromotionTarget -> GameContext -> Either MoveError GameContext
playPromotion move pt = playMove move (Just pt) Nothing

resign :: Color -> GameContext -> GameContext
resign White = setResult BlackWin
resign Black = setResult WhiteWin

makeDraw :: GameContext -> GameContext
makeDraw = setResult Draw

setResult :: Result -> GameContext -> GameContext
setResult result gc = gc {
    mainHeaderData = setResultHeader result (mainHeaderData gc),
    gameResult = result
}

createMinimallySpecifiedMoves :: GameContext -> Either MoveError [UnderspecifiedMove]
createMinimallySpecifiedMoves gc = zipWithM createMinimallySpecifiedMove (positions gc) (moves gc)

updateGameContext :: MoveContext -> PositionContext -> GameContext -> GameContext
updateGameContext mc pc gc =
    setResult (getNewResult pc) $ gc {
        currentPosition = pc,
        positions = positions gc ++ [pc],
        moves = moves gc ++ [mc]
    }

getNewResult :: PositionContext -> Result
getNewResult pc
    | isCheckmate pc = getWin (otherColor (currentPlayer pc))
    | isStalemate pc = Draw
    | otherwise = Ongoing

setResultHeader :: Result -> HeaderData -> HeaderData
setResultHeader result hd = hd { resultHeader = result }
