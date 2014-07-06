{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.Pgn (
    readPgn,
    writePgn
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Formats.Common
import Chesskel.Gameplay
import Control.Applicative hiding ((<|>), many)
import Data.Maybe
import Text.ParserCombinators.Parsec

data PgnError =
    PgnSyntaxError String |
    InvalidMoveError String deriving (Eq, Show)

type HeaderToken = (String, String)

data GameToken = Game MoveNumberToken | EmptyGame
data MoveNumberToken = MoveNumber Int WhiteMoveToken | FinalMoveNumber Int GameResultToken
data WhiteMoveToken = WhiteMove PgnMove BlackMoveToken | FinalWhiteMove PgnMove GameResultToken
data BlackMoveToken = BlackMove PgnMove MoveNumberToken | FinalBlackMove PgnMove GameResultToken
data GameResultToken = GameResult Result

data PgnMove = PgnCastleMove CastlingDirection | PgnMove {
    pgnChessman :: Chessman,
    pgnToCell :: Cell,
    pgnIsCapture :: Bool,
    pgnFromFile :: Maybe File,
    pgnFromRank :: Maybe Rank,
    pgnPromotionTarget :: Maybe PromotionTarget,
    pgnCheckState :: Maybe CheckState
} deriving (Eq)

instance Show GameToken where
    showsPrec _ EmptyGame = showString ""
    showsPrec _ (Game moveNumberToken) = shows moveNumberToken

instance Show MoveNumberToken where
    showsPrec _ (FinalMoveNumber moveNum resultToken) = shows moveNum . showString ". " . shows resultToken
    showsPrec _ (MoveNumber moveNum whiteMoveToken) = shows moveNum . showString ". " . shows whiteMoveToken

instance Show WhiteMoveToken where
    showsPrec _ (FinalWhiteMove mv resultToken) = shows mv . showString " " . shows resultToken
    showsPrec _ (WhiteMove mv blackMoveToken) = shows mv . showString " " . shows blackMoveToken

instance Show BlackMoveToken where
    showsPrec _ (FinalBlackMove mv resultToken) = shows mv . showString " " . shows resultToken
    showsPrec _ (BlackMove mv moveNumberToken) = shows mv . showString " " . shows moveNumberToken

instance Show GameResultToken where
    showsPrec _ (GameResult res) = shows res

instance Show PgnMove where
    showsPrec _ (PgnCastleMove direction) = shows direction
    showsPrec _ mv = showChessman . showFromFile . showFromRank . showCapture . showToCell . showPromotion . showCheck where
        showChessman = sChessman (pgnChessman mv)
        showFromFile = maybe showEmpty sFile (pgnFromFile mv)
        showFromRank = maybe showEmpty sRank (pgnFromRank mv)
        showCapture = if pgnIsCapture mv then showString "x" else showEmpty
        showToCell = let Cell (f, r) = pgnToCell mv in sFile f . sRank r
        showPromotion = maybe showEmpty sPromotion (pgnPromotionTarget mv)
        showCheck = maybe showEmpty sCheck (pgnCheckState mv)
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

quote = char '"'
headerTagValue = quote *> many (noneOf "\"") <* quote
headerTag = many1 alphaNum

header = do
    many (char ' ')
    char '['
    tag <- headerTag
    many1 (char ' ')
    value <- headerTagValue
    char ']'
    many (char ' ')
    return ((tag, value) :: HeaderToken)

headers = many (header <* newline)

interpretHeaders :: [HeaderToken] -> Either PgnError AllHeaderData
interpretHeaders = go unknownHeaderData [] where
    go :: HeaderData -> [ExtraHeader] -> [HeaderToken] -> Either PgnError AllHeaderData
    go hd extra [] = Right (hd, reverse extra)
    go hd extra (x:xs) = do
        let (tag, value) = x
        (hd', isMainTag) <- updateHeaderData hd tag value
        if isMainTag then go hd' extra xs else go hd (uncurry EH x:extra) xs

updateHeaderData :: HeaderData -> String -> String -> Either PgnError (HeaderData, Bool)
updateHeaderData hd tag value = case tag of
    -- TODO: lenses or something?
    "Event"  -> Right (hd { eventHeader = value }, True)
    "Site"   -> Right (hd { siteHeader  = value }, True)
    "Date"   -> Right (hd { dateHeader  = value }, True)
    "Round"  -> Right (hd { roundHeader = value }, True)
    "White"  -> Right (hd { whiteHeader = value }, True)
    "Black"  -> Right (hd { blackHeader = value }, True)
    "Result" -> flip (,) True <$> setResultOrFail hd value
    _ -> Right (hd, False)

parseResult resultStr = either (const Nothing) Just $ parse result "ParseResult" resultStr

setResultOrFail :: HeaderData -> String -> Either PgnError HeaderData
setResultOrFail hd value = maybe (Left $ PgnSyntaxError "Invalid result") Right $ do
    res <- parseResult value
    return $ hd { resultHeader = res }

king = King <$ char 'K'
queen = Queen <$ char 'Q'
rook = Rook <$ char 'R'
bishop = Bishop <$ char 'B'
knight = Knight <$ char 'N'
pawn = Pawn <$ char 'P' -- Rarely used notation, but legal.
chessman = king <|> queen <|> rook <|> bishop <|> knight <|> pawn

check = Check <$ char '+'
checkmate = Checkmate <$ char '#'
checkState = check <|> checkmate

pQueen = PQueen <$ char 'Q'
pRook = PRook <$ char 'R'
pBishop = PBishop <$ char 'B'
pKnight = PKnight <$ char 'N'
pTarget = pQueen <|> pRook <|> pBishop <|> pKnight

pawnPromotion = char '=' *> pTarget

capture = () <$ char 'x'

ongoing = Ongoing <$ string "*"
whiteWin = WhiteWin <$ string "1-0"
blackWin = BlackWin <$ string "0-1"
draw = Draw <$ (string "1/2-1/2" <|> string "½-½")

-- We need to backtrack after whiteWin because it starts with the same character as draw.
-- The reason we backtrack after blackWin is more subtle: It's because it may start with the same characters as castling.
-- The parser needs to handle both "4. 0-0 0-0" (both sides castle short) and "4. 0-0 0-1" (white castles short, then resigns).
result = ongoing <|> try whiteWin <|> try blackWin <|> draw

moveNumber :: Parser Int
moveNumber = do
    d <- many1 digit
    char '.'
    return (read d)

pawnCapture = try (file <* capture) >>= pawnMoveBody . Just

pawnNonCapture = pawnMoveBody Nothing

pawnMoveBody mFromFile = do
    toCell <- cell
    mPromotionTarget <- optionMaybe pawnPromotion
    mCheckState <- optionMaybe checkState
    return PgnMove {
        -- For pawns we have the convenient invariant that the move is a capture iff there's a file disambiguation.
        -- Rank disambiguations are also never applicable for pawns.
        pgnChessman = Pawn,
        pgnToCell = toCell,
        pgnIsCapture = isJust mFromFile,
        pgnFromFile = mFromFile,
        pgnFromRank = Nothing,
        pgnPromotionTarget = mPromotionTarget,
        pgnCheckState = mCheckState
    }

pawnMove = pawnCapture <|> pawnNonCapture

bodyEnd checkCapture mFromFile mFromRank = do
    -- If checkCapture is false we always set mCapture to Nothing.
    mCapture <- if checkCapture then Just <$> capture else return Nothing
    toCell <- cell
    return (mFromFile, mFromRank, isJust mCapture, toCell)

noDisambiguation checkCapture = bodyEnd checkCapture Nothing Nothing
fileDisambiguation checkCapture = file >>= \fromFile -> bodyEnd checkCapture (Just fromFile) Nothing
rankDisambiguation checkCapture = rank >>= \fromRank -> bodyEnd checkCapture Nothing (Just fromRank)
fileAndRankDisambiguation checkCapture = file >>= \fromFile -> rank >>= \fromRank -> bodyEnd checkCapture (Just fromFile) (Just fromRank)

justCapture = noDisambiguation True
justCaptureWithFile = fileDisambiguation True
justCaptureWithRank = rankDisambiguation True
justCaptureWithFileAndRank = fileAndRankDisambiguation True

nonCapture = noDisambiguation False
nonCaptureWithFile = fileDisambiguation False
nonCaptureWithRank = rankDisambiguation False
nonCaptureWithFileAndRank = fileAndRankDisambiguation False

nonPawnCaptureBody = try justCaptureWithFileAndRank <|>
                     try justCaptureWithFile <|>
                     try justCaptureWithRank <|>
                     try justCapture

nonPawnNonCaptureBody = try nonCaptureWithFileAndRank <|>
                        try nonCaptureWithFile <|>
                        try nonCaptureWithRank <|>
                        try nonCapture

nonPawnMoveBody = nonPawnCaptureBody <|> nonPawnNonCaptureBody

nonPawnMove = do
    cm <- chessman
    (mFromFile, mFromRank, moveIsCapture, toCell) <- nonPawnMoveBody
    mCheckState <- optionMaybe checkState
    return PgnMove {
        pgnChessman = cm,
        pgnToCell = toCell,
        pgnIsCapture = moveIsCapture,
        pgnFromFile = mFromFile,
        pgnFromRank = mFromRank,
        pgnPromotionTarget = Nothing,
        pgnCheckState = mCheckState
    }

castleShort = PgnCastleMove Kingside <$ (string "O-O" <|> string "0-0")
castleLong = PgnCastleMove Queenside <$ (string "O-O-O" <|> string "0-0-0")
castling = try castleLong <|> castleShort

ply = castling <|> pawnMove <|> nonPawnMove

middleWhiteMove = middleMove WhiteMove blackMove
middleBlackMove = middleMove BlackMove astMoveNumber
middleMove tokenType next = do
    mv <- ply
    many1 space
    nextToken <- next
    return $ tokenType mv nextToken

finalWhiteMove = finalMove FinalWhiteMove
finalBlackMove = finalMove FinalBlackMove
finalMove tokenType = do
    mv <- ply
    many1 space
    res <- result
    return (tokenType mv (GameResult res))

middleMoveNumber = do
    moveNum <- moveNumber
    many space
    whiteMoveToken <- whiteMove
    return (MoveNumber moveNum whiteMoveToken)

finalMoveNumber = do
    moveNum <- moveNumber
    many space
    res <- result
    return (FinalMoveNumber moveNum (GameResult res))

whiteMove = try finalWhiteMove <|> middleWhiteMove
blackMove = try finalBlackMove <|> middleBlackMove
astMoveNumber = try finalMoveNumber <|> middleMoveNumber

movetext = EmptyGame <$ eof <|>
           Game <$> astMoveNumber

pgn = (,) <$> headers <*> (many newline >> movetext)

mapSyntaxError :: Either ParseError a -> Either PgnError a
mapSyntaxError (Right a) = Right a
mapSyntaxError (Left e) = Left $ PgnSyntaxError $ "Invalid syntax: " ++ show e

mapGameplayError :: Int -> Color -> PgnMove -> Either MoveError a -> Either PgnError a
mapGameplayError _ _ _ (Right a) = Right a
mapGameplayError moveN color pgnMove (Left e) =
    let dots = if color == White then "." else "..."
        moveStr = show moveN ++ dots ++ " " ++ show pgnMove in
    Left $ InvalidMoveError $ "Invalid move: " ++ moveStr ++ " (" ++ show e ++ ")"

createUnderspecifiedMove :: PositionContext -> PgnMove -> UnderspecifiedMove
createUnderspecifiedMove _ (PgnCastleMove direction) = CastleMove direction
createUnderspecifiedMove pc pgnMove = UM {
    knownToCell = pgnToCell pgnMove,
    knownPiece = (pgnChessman pgnMove, currentPlayer pc),
    knownFromFile = pgnFromFile pgnMove,
    knownFromRank = pgnFromRank pgnMove,
    knownIsCapture = pgnIsCapture pgnMove,
    knownPromotionTarget = pgnPromotionTarget pgnMove
}

interpretMove :: Int -> Color -> PgnMove -> GameContext -> Either PgnError GameContext
interpretMove moveN color pgnMove gc = 
    mapGameplayError moveN color pgnMove $ playUnderspecifiedMove (createUnderspecifiedMove (currentPosition gc) pgnMove) gc

interpretGameResult :: GameResultToken -> GameContext -> GameContext
interpretGameResult (GameResult res) = setResult res

-- We don't do any validation of the move numbers yet.
interpretMoveNumber :: MoveNumberToken -> GameContext -> Either PgnError GameContext
interpretMoveNumber (FinalMoveNumber _ resultToken) gc = return $ interpretGameResult resultToken gc
interpretMoveNumber (MoveNumber moveN whiteMoveToken) gc = interpretWhiteMove moveN whiteMoveToken gc

interpretWhiteMove :: Int -> WhiteMoveToken -> GameContext -> Either PgnError GameContext
interpretWhiteMove moveN (FinalWhiteMove mv resultToken) gc = interpretFinalMove moveN White resultToken mv gc
interpretWhiteMove moveN (WhiteMove mv blackMoveToken) gc = interpretMove moveN White mv gc >>= interpretBlackMove moveN blackMoveToken

interpretBlackMove :: Int -> BlackMoveToken -> GameContext -> Either PgnError GameContext
interpretBlackMove moveN (FinalBlackMove mv resultToken) gc = interpretFinalMove moveN Black resultToken mv gc
interpretBlackMove moveN (BlackMove mv moveNumberToken) gc = interpretMove moveN Black mv gc >>= interpretMoveNumber moveNumberToken

interpretFinalMove :: Int -> Color -> GameResultToken -> PgnMove -> GameContext -> Either PgnError GameContext
interpretFinalMove moveN color resultToken mv gc = interpretGameResult resultToken <$> interpretMove moveN color mv gc

-- We currently don't support partial games, i.e. games where the first move in the text isn't move 1.
interpretGame :: GameToken -> AllHeaderData -> Either PgnError GameContext
interpretGame EmptyGame hd = return (startGame startPosition hd)
interpretGame (Game moveNumberToken) hd = interpretMoveNumber moveNumberToken (startGame startPosition hd)

createBlackMoveToken :: Int -> Result -> PgnMove -> [PgnMove] -> BlackMoveToken
createBlackMoveToken _ res x [] = FinalBlackMove x (GameResult res)
createBlackMoveToken moveN res x xs = BlackMove x (createMoveNumberToken (moveN+1) res xs)

createWhiteMoveToken :: Int -> Result -> PgnMove -> [PgnMove] -> WhiteMoveToken
createWhiteMoveToken _ res x [] = FinalWhiteMove x (GameResult res)
createWhiteMoveToken moveN res x (y:ys) = WhiteMove x (createBlackMoveToken moveN res y ys)

createMoveNumberToken :: Int -> Result -> [PgnMove] -> MoveNumberToken
createMoveNumberToken moveN res [] = FinalMoveNumber moveN (GameResult res)
createMoveNumberToken moveN res (x:xs) = MoveNumber moveN (createWhiteMoveToken moveN res x xs)

createGameToken :: Result -> [PgnMove] -> GameToken
createGameToken _ [] = EmptyGame
createGameToken res xs = Game (createMoveNumberToken 1 res xs)

createPgnMove :: PositionContext -> UnderspecifiedMove -> PgnMove
createPgnMove _ (CastleMove direction) = PgnCastleMove direction
createPgnMove nextPosition unspecMove = 
    let (cm, _) = knownPiece unspecMove in
    PgnMove {
        pgnChessman = cm,
        pgnToCell = knownToCell unspecMove,
        pgnIsCapture = knownIsCapture unspecMove,
        pgnFromFile = knownFromFile unspecMove,
        pgnFromRank = knownFromRank unspecMove,
        pgnPromotionTarget = knownPromotionTarget unspecMove,
        pgnCheckState = getCheckState nextPosition
    }

createPgnMoves :: GameContext -> Either MoveError [PgnMove]
createPgnMoves gc
    | null (positions gc) = return []
    | otherwise = do
        unspecMoves <- createMinimallySpecifiedMoves gc
        -- We skip the first move because createPgnMove needs each move paired with the _next_ position,
        -- and the first position in the position list always precedes the first move in the move list.
        return $ zipWith createPgnMove (tail (positions gc)) unspecMoves

-- TODO: should divide into lines of less than 80 characters.
writeGameToken :: GameToken -> ShowS
writeGameToken = shows

writeExtraHeaders :: [ExtraHeader] -> ShowS
writeExtraHeaders [] = showString ""
writeExtraHeaders (x:xs) = shows x . showString "\n" . writeExtraHeaders xs

writeAllHeaderData :: AllHeaderData -> ShowS
writeAllHeaderData (hd, extra) = shows hd . writeExtraHeaders extra

writeGame :: AllHeaderData -> GameToken -> String
writeGame hdata gameToken = writeAllHeaderData hdata . showString "\n\n" . writeGameToken gameToken $ ""

createGame :: GameContext -> Either MoveError GameToken
createGame gc = createGameToken (gameResult gc) <$> createPgnMoves gc

readPgn :: String -> Either PgnError GameContext
readPgn pgnString = do
    (headerTokens, gameToken) <- mapSyntaxError (parse pgn "ReadPgn" pgnString)
    hdata <- interpretHeaders headerTokens
    interpretGame gameToken hdata

writePgn :: GameContext -> Either MoveError String
writePgn gc = writeGame (allHeaderData gc) <$> createGame gc
