{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.Pgn (
    readPgn
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Gameplay
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Maybe
import Text.ParserCombinators.Parsec

data PgnError =
    PgnSyntaxError String |
    InvalidMoveError String deriving (Eq, Show)

type HeaderToken = (String, String)

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

data CheckState = Check | Checkmate deriving (Eq, Show)

data GameToken = Game MoveNumberToken | EmptyGame
data MoveNumberToken = MoveNumber Int WhiteMoveToken | FinalMoveNumber Int GameResultToken
data WhiteMoveToken = WhiteMove PgnMove BlackMoveToken | FinalWhiteMove PgnMove GameResultToken
data BlackMoveToken = BlackMove PgnMove MoveNumberToken | FinalBlackMove PgnMove GameResultToken
data GameResultToken = GameResult Result

data PgnMove = CastleMove CastlingDirection | PgnMove {
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
    showsPrec _ (CastleMove direction) = shows direction
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

fileA = FileA <$ char 'a'
fileB = FileB <$ char 'b'
fileC = FileC <$ char 'c'
fileD = FileD <$ char 'd'
fileE = FileE <$ char 'e'
fileF = FileF <$ char 'f'
fileG = FileG <$ char 'g'
fileH = FileH <$ char 'h'
file = fileA <|> fileB <|> fileC <|> fileD <|> fileE <|> fileF <|> fileG <|> fileH

rank1 = Rank1 <$ char '1'
rank2 = Rank2 <$ char '2'
rank3 = Rank3 <$ char '3'
rank4 = Rank4 <$ char '4'
rank5 = Rank5 <$ char '5'
rank6 = Rank6 <$ char '6'
rank7 = Rank7 <$ char '7'
rank8 = Rank8 <$ char '8'
rank = rank1 <|> rank2 <|> rank3 <|> rank4 <|> rank5 <|> rank6 <|> rank7 <|> rank8

cell = createCell <$> file <*> rank

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

castleShort = CastleMove Kingside <$ (string "O-O" <|> string "0-0")
castleLong = CastleMove Queenside <$ (string "O-O-O" <|> string "0-0-0")
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

mapGameplayError :: Either MoveError a -> Either PgnError a
mapGameplayError (Right a) = Right a
mapGameplayError (Left e) = Left $ InvalidMoveError $ "Invalid move: " ++ show e

getSourceCell :: PositionContext -> PgnMove -> Either PgnError Cell
getSourceCell (PC { currentPlayer = White }) (CastleMove _) = return e1
getSourceCell (PC { currentPlayer = Black }) (CastleMove _) = return e8
getSourceCell pc pgnMove =
    case createCell <$> pgnFromFile pgnMove <*> pgnFromRank pgnMove of
        Just c -> return c
        Nothing -> disambiguateSourceCell pc pgnMove

disambiguateSourceCell :: PositionContext -> PgnMove -> Either PgnError Cell
disambiguateSourceCell pc pgnMove =
    let toCell = pgnToCell pgnMove
        piece = (pgnChessman pgnMove, currentPlayer pc)
        mFromFile = pgnFromFile pgnMove
        mFromRank = pgnFromRank pgnMove
        allCandidates = findCandidateSourceCells pc toCell piece mFromFile mFromRank in
    case allCandidates of
        [single] -> Right single
        [] -> Left $ PgnSyntaxError $ "No piece found for move " ++ show pgnMove
        multiple -> Left $ PgnSyntaxError $
            "Multiple pieces found for move " ++
            show pgnMove ++
            ". The following source cells are possible: " ++
            show multiple ++
            "."

findCandidateSourceCells :: PositionContext -> Cell -> Piece -> Maybe File -> Maybe Rank -> [Cell]
findCandidateSourceCells pc toCell piece mFromFile mFromRank = do
    MC { mainFromCell = fromCell, mainToCell = toCell' } <- findAllLegalMoves pc
    guard $ toCell == toCell'
    let Cell (f, r) = fromCell
    guard $ maybe True (== f) mFromFile
    guard $ maybe True (== r) mFromRank
    guard $ hasPieceOfType piece (position pc) fromCell
    return fromCell

getDestinationCell :: PositionContext -> PgnMove -> Cell
getDestinationCell (PC { currentPlayer = White }) (CastleMove Queenside) = c1
getDestinationCell (PC { currentPlayer = White }) (CastleMove Kingside) = g1
getDestinationCell (PC { currentPlayer = Black }) (CastleMove Queenside) = c8
getDestinationCell (PC { currentPlayer = Black }) (CastleMove Kingside) = g8
getDestinationCell _ pgnMove = pgnToCell pgnMove

translateMove :: PositionContext -> PgnMove -> Either PgnError (Move, Maybe PromotionTarget)
translateMove pc pgnMove = do
    sourceCell <- getSourceCell pc pgnMove
    let destinationCell = getDestinationCell pc pgnMove
    return (createMove sourceCell destinationCell, safeGetPromotionTarget pgnMove) where
        -- TODO: This is necessary because of the way the PgnMove type is currently defined.
        -- It should perhaps be split into two types -- one for castling moves and one for regular moves.
        safeGetPromotionTarget (CastleMove _) = Nothing
        safeGetPromotionTarget mv = pgnPromotionTarget mv

interpretMove :: GameContext -> PgnMove -> Either PgnError GameContext
interpretMove gc pgnMove = do
    (move, mPromotionTarget) <- translateMove (currentPosition gc) pgnMove
    mapGameplayError (playMove move mPromotionTarget gc)

-- We currently don't support partial games, i.e. games where the first move in the text isn't move 1.
interpretGame :: AllHeaderData -> GameToken -> Either PgnError GameContext
interpretGame hd EmptyGame = return (startGame startPosition hd)
interpretGame hd (Game moveNumberToken) = interpretMoveNumber (startGame startPosition hd) moveNumberToken

interpretGameResult :: GameContext -> GameResultToken -> GameContext
interpretGameResult gc (GameResult res) = setResult res gc

-- We also don't do any validation of the move numbers yet.
interpretMoveNumber :: GameContext -> MoveNumberToken -> Either PgnError GameContext
interpretMoveNumber gc (FinalMoveNumber _ resultToken) = return $ interpretGameResult gc resultToken
interpretMoveNumber gc (MoveNumber _ whiteMoveToken) = interpretWhiteMove gc whiteMoveToken

interpretWhiteMove :: GameContext -> WhiteMoveToken -> Either PgnError GameContext
interpretWhiteMove gc (FinalWhiteMove mv resultToken) = interpretFinalMove gc mv resultToken
interpretWhiteMove gc (WhiteMove mv blackMoveToken) = interpretMove gc mv >>= \gc' -> interpretBlackMove gc' blackMoveToken

interpretBlackMove :: GameContext -> BlackMoveToken -> Either PgnError GameContext
interpretBlackMove gc (FinalBlackMove mv resultToken) = interpretFinalMove gc mv resultToken
interpretBlackMove gc (BlackMove mv moveNumberToken) = interpretMove gc mv >>= \gc' -> interpretMoveNumber gc' moveNumberToken

interpretFinalMove :: GameContext -> PgnMove -> GameResultToken -> Either PgnError GameContext
interpretFinalMove gc mv resultToken = interpretMove gc mv >>= \gc' -> return $ interpretGameResult gc' resultToken

readPgn :: String -> Either PgnError GameContext
readPgn pgnString = do
    (headerTokens, gameToken) <- mapSyntaxError (parse pgn "ReadPgn" pgnString)
    allHeaderData <- interpretHeaders headerTokens
    interpretGame allHeaderData gameToken
