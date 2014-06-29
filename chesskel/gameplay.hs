module Chesskel.Gameplay (
    GameContext (..),
    HeaderData (..),
    Result (..),
    startStandardGame,
    startGame,
    playMove,
    playNonPromotionMove,
    playPromotion,
    resign,
    draw
) where

import Chesskel.Board
import Chesskel.Movement

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
    (mc, pc') <- makeMove' (currentPosition gc) move mpt
    return $ updateGameContext mc pc' gc

playNonPromotionMove :: Move -> GameContext -> Either MoveError GameContext
playNonPromotionMove move = playMove move Nothing

playPromotion :: Move -> PromotionTarget -> GameContext -> Either MoveError GameContext
playPromotion move pt = playMove move (Just pt)

resign :: Color -> GameContext -> GameContext
resign color gc = gc {
    mainHeaderData = setWinner (otherColor color) (mainHeaderData gc)
}

draw :: GameContext -> GameContext
draw gc = gc {
    mainHeaderData = setDraw (mainHeaderData gc)
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
setWinner White = setResult WhiteWin
setWinner Black = setResult BlackWin

setDraw :: HeaderData -> HeaderData
setDraw = setResult Draw

setResult :: Result -> HeaderData -> HeaderData
setResult result hd = hd { resultHeader = result }
