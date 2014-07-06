module Chesskel.Gameplay (
    GameContext (..),
    HeaderData (..),
    AllHeaderData,
    ExtraHeader (..),
    Result (..),
    unknownHeaderData,
    startStandardGame,
    startGame,
    playMove,
    playUnderspecifiedMove,
    playNonPromotionMove,
    playPromotion,
    resign,
    makeDraw,
    setResult
) where

import Chesskel.Board
import Chesskel.Movement

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
    extraHeaderData :: [ExtraHeader]
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

startStandardGame :: GameContext
startStandardGame = startGame startPosition (unknownHeaderData, [])

startGame :: PositionContext -> AllHeaderData -> GameContext
startGame pc (hd, extra) = GC {
    currentPosition = pc,
    positions = [],
    moves = [],
    mainHeaderData = hd,
    extraHeaderData = extra
}

playMove :: Move -> Maybe PromotionTarget -> GameContext -> Either MoveError GameContext
playMove move mpt gc = do
    (mc, pc') <- makeMove (currentPosition gc) move mpt
    return $ updateGameContext mc pc' gc

playUnderspecifiedMove :: UnderspecifiedMove -> GameContext -> Either MoveError GameContext
playUnderspecifiedMove unspecMove gc = do
    (mc, pc') <- makeUnderspecifiedMove (currentPosition gc) unspecMove
    return $ updateGameContext mc pc' gc

playNonPromotionMove :: Move -> GameContext -> Either MoveError GameContext
playNonPromotionMove move = playMove move Nothing

playPromotion :: Move -> PromotionTarget -> GameContext -> Either MoveError GameContext
playPromotion move pt = playMove move (Just pt)

resign :: Color -> GameContext -> GameContext
resign White = setResult BlackWin
resign Black = setResult WhiteWin

makeDraw :: GameContext -> GameContext
makeDraw = setResult Draw

setResult :: Result -> GameContext -> GameContext
setResult result gc = gc {
    mainHeaderData = setResultHeader result (mainHeaderData gc)
}

updateGameContext :: MoveContext -> PositionContext -> GameContext -> GameContext
updateGameContext mc pc gc = gc {
    currentPosition = pc,
    positions = currentPosition gc:positions gc,
    moves = mc:moves gc,
    mainHeaderData = updateHeaderDataIfFinished pc (mainHeaderData gc)
}

updateHeaderDataIfFinished :: PositionContext -> HeaderData -> HeaderData
updateHeaderDataIfFinished pc hd
    | isCheckmate pc = setWinner (otherColor (currentPlayer pc)) hd
    | isStalemate pc = setDraw hd
    | otherwise = hd

setWinner :: Color -> HeaderData -> HeaderData
setWinner White = setResultHeader WhiteWin
setWinner Black = setResultHeader BlackWin

setDraw :: HeaderData -> HeaderData
setDraw = setResultHeader Draw

setResultHeader :: Result -> HeaderData -> HeaderData
setResultHeader result hd = hd { resultHeader = result }
