{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Chesskel.Formats.Pgn (
    PgnError (..),
    readPgn,
    writePgn
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Formats.Common
import Chesskel.Gameplay
import Chesskel.Utils
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

-- |An error indicating that there's something wrong with a PGN.
data PgnError =
    -- |The PGN has a syntax error.
    PgnSyntaxError String |
    
    -- |The PGN is syntactically valid, but contains one or more invalid moves.
    InvalidMoveError String deriving (Eq, Show)

type HeaderToken = (String, String)

data GameToken = Game MoveNumberToken | EmptyGame deriving (Eq)
data MoveNumberToken = MoveNumber MoveNum WhiteMoveToken | FinalMoveNumber MoveNum GameResultToken deriving (Eq)
data WhiteMoveToken = WhiteMove MoveToken BlackMoveToken | FinalWhiteMove MoveToken GameResultToken deriving (Eq)
data BlackMoveToken = BlackMove MoveToken MoveNumberToken | FinalBlackMove MoveToken GameResultToken deriving (Eq)
data MoveToken = AnnotatedMove PgnMove AnnotationToken | UnannotatedMove PgnMove deriving (Eq)
data AnnotationToken = Annotation PgnAnnotation deriving (Eq)
data GameResultToken = GameResult Result deriving (Eq)

newtype MoveNum = MN Int deriving (Eq, Ord, Bounded)
newtype PgnAnnotation = MkPgnAnnotation T.Text deriving (Eq, Ord)

data PgnMove = PgnCastleMove CastlingDirection | PgnMove {
    pgnChessman :: Chessman,
    pgnToCell :: Cell,
    pgnIsCapture :: Bool,
    pgnFromFile :: Maybe File,
    pgnFromRank :: Maybe Rank,
    pgnPromotionTarget :: Maybe PromotionTarget,
    pgnCheckState :: Maybe CheckState
} deriving (Eq)

type AnnotatedPgnMove = (PgnMove, Maybe PgnAnnotation)

data GameItem = MoveNumItem MoveNum |
                PgnMoveItem PgnMove |
                AnnotatedPgnMoveItem PgnMove PgnAnnotation |
                ResultItem Result deriving (Eq)

class Token t where
    gameItems :: t -> [GameItem]

class LeafToken t where
    gameItem :: t -> GameItem

instance Token GameToken where
    gameItems EmptyGame = []
    gameItems (Game moveNumberToken) = gameItems moveNumberToken

instance Token MoveNumberToken where
    gameItems (FinalMoveNumber moveNum resultToken) = [gameItem moveNum, gameItem resultToken]
    gameItems (MoveNumber moveNum whiteMoveToken) = gameItem moveNum : gameItems whiteMoveToken

instance Token WhiteMoveToken where
    gameItems (FinalWhiteMove moveToken resultToken) = [gameItem moveToken, gameItem resultToken]
    gameItems (WhiteMove moveToken blackMoveToken) = gameItem moveToken : gameItems blackMoveToken

instance Token BlackMoveToken where
    gameItems (FinalBlackMove moveToken resultToken) = [gameItem moveToken, gameItem resultToken]
    gameItems (BlackMove moveToken moveNumberToken) = gameItem moveToken : gameItems moveNumberToken

instance LeafToken MoveToken where
    gameItem (AnnotatedMove mv (Annotation ann)) = AnnotatedPgnMoveItem mv ann
    gameItem (UnannotatedMove mv) = PgnMoveItem mv

instance LeafToken MoveNum where
    gameItem = MoveNumItem

instance LeafToken GameResultToken where
    gameItem (GameResult res) = ResultItem res

instance Show MoveNum where
    show (MN n) = show n ++ "."

instance Show PgnMove where
    showsPrec n (PgnCastleMove direction) = showsPrec n direction
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

instance Show PgnAnnotation where
    show (MkPgnAnnotation s) = "{" ++ T.unpack s ++ "}"

instance Show GameItem where
    show (MoveNumItem moveNum) = show moveNum
    show (PgnMoveItem pgnMove) = show pgnMove
    show (AnnotatedPgnMoveItem pgnMove ann) = show pgnMove ++ " " ++ show ann
    show (ResultItem res) = show res

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
chessman = king <|> queen <|> rook <|> bishop <|> knight <|> pawn <?>
    "chessman (K=King, Q=Queen, R=Rook, B=Bishop, N=Knight or P=Pawn)"

check = Check <$ char '+'
checkmate = Checkmate <$ char '#'
checkState = check <|> checkmate <?> "check indicator (+ = check, # = checkmate)"

pQueen = PQueen <$ char 'Q'
pRook = PRook <$ char 'R'
pBishop = PBishop <$ char 'B'
pKnight = PKnight <$ char 'N'
pTarget = pQueen <|> pRook <|> pBishop <|> pKnight <?>
    "promotion target (Q=Queen, R=Rook, B=Bishop or N=Knight)"

pawnPromotion = char '=' *> pTarget

capture = void (char 'x')

ongoing = Ongoing <$ string "*"
whiteWin = WhiteWin <$ string "1-0"
blackWin = BlackWin <$ string "0-1"
draw = Draw <$ (string "1/2-1/2" <|> string "½-½")

-- We need to backtrack after whiteWin because it starts with the same character as draw.
-- The reason we backtrack after blackWin is more subtle: It's because it may start with the same characters as castling.
-- The parser needs to handle both "4. 0-0 0-0" (both sides castle short) and "4. 0-0 0-1" (white castles short, then resigns).
result = ongoing <|> try whiteWin <|> try blackWin <|> draw <?>
    "game result (* = ongoing, 1-0 = white win, 0-1 = black win, 1/2-1/2 = draw)"

moveNumber :: Parser MoveNum
moveNumber = do
    d <- many1 digit
    char '.'
    return $ MN $ read d

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

annotation = do
    char '{'
    ann <- many (noneOf "{}")
    char '}'
    many1 space
    return $ MkPgnAnnotation (T.replace (T.pack "\n") (T.pack " ") (T.pack ann))

moveAndAnnotation = do
    mv <- ply
    many1 space
    mAnnotation <- optionMaybe annotation
    case mAnnotation of
        Nothing -> return $ UnannotatedMove mv
        Just ann -> return $ AnnotatedMove mv (Annotation ann)

middleWhiteMove = middleMove WhiteMove blackMove
middleBlackMove = middleMove BlackMove astMoveNumber
middleMove tokenType next = tokenType <$> moveAndAnnotation <*> next

finalWhiteMove = finalMove FinalWhiteMove
finalBlackMove = finalMove FinalBlackMove
finalMove tokenType = do
    moveToken <- moveAndAnnotation
    res <- result
    return $ tokenType moveToken (GameResult res)

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

mapPgnAnnotation :: PgnAnnotation -> MoveAnnotation
mapPgnAnnotation (MkPgnAnnotation s) = MA s

mapMoveAnnotation :: MoveAnnotation -> PgnAnnotation
mapMoveAnnotation (MA s) = MkPgnAnnotation s

createUnderspecifiedMove :: PositionContext -> AnnotatedPgnMove -> UnderspecifiedMove
createUnderspecifiedMove _ (PgnCastleMove direction, mAnnotation) = CastleMove direction (fmap mapPgnAnnotation mAnnotation)
createUnderspecifiedMove pc (pgnMove, mAnnotation) = UM {
    knownToCell = pgnToCell pgnMove,
    knownPiece = (pgnChessman pgnMove, currentPlayer pc),
    knownFromFile = pgnFromFile pgnMove,
    knownFromRank = pgnFromRank pgnMove,
    knownIsCapture = pgnIsCapture pgnMove,
    knownPromotionTarget = pgnPromotionTarget pgnMove,
    knownMoveAnnotation = fmap mapPgnAnnotation mAnnotation
}

interpretPgnMove :: Int -> Color -> AnnotatedPgnMove -> GameContext -> Either PgnError GameContext
interpretPgnMove moveN color (pgnMove, mAnnotation) gc =
    mapGameplayError moveN color pgnMove $
        playUnderspecifiedMove (createUnderspecifiedMove (currentPosition gc) (pgnMove, mAnnotation)) gc

interpretMove :: Int -> Color -> MoveToken -> GameContext -> Either PgnError GameContext
interpretMove moveN color (AnnotatedMove pgnMove (Annotation ann)) gc = interpretPgnMove moveN color (pgnMove, Just ann) gc
interpretMove moveN color (UnannotatedMove pgnMove) gc = interpretPgnMove moveN color (pgnMove, Nothing) gc

interpretGameResult :: GameResultToken -> GameContext -> GameContext
interpretGameResult (GameResult res) = setResult res

-- We don't do any validation of the move numbers yet.
interpretMoveNumber :: MoveNumberToken -> GameContext -> Either PgnError GameContext
interpretMoveNumber (FinalMoveNumber _ resultToken) gc = return $ interpretGameResult resultToken gc
interpretMoveNumber (MoveNumber (MN moveN) whiteMoveToken) gc = interpretWhiteMove moveN whiteMoveToken gc

interpretWhiteMove :: Int -> WhiteMoveToken -> GameContext -> Either PgnError GameContext
interpretWhiteMove moveN (FinalWhiteMove moveToken resultToken) gc = interpretFinalMove moveN White resultToken moveToken gc
interpretWhiteMove moveN (WhiteMove moveToken blackMoveToken) gc = interpretMove moveN White moveToken gc >>= interpretBlackMove moveN blackMoveToken

interpretBlackMove :: Int -> BlackMoveToken -> GameContext -> Either PgnError GameContext
interpretBlackMove moveN (FinalBlackMove moveToken resultToken) gc = interpretFinalMove moveN Black resultToken moveToken gc
interpretBlackMove moveN (BlackMove moveToken moveNumberToken) gc = interpretMove moveN Black moveToken gc >>= interpretMoveNumber moveNumberToken

interpretFinalMove :: Int -> Color -> GameResultToken -> MoveToken -> GameContext -> Either PgnError GameContext
interpretFinalMove moveN color resultToken moveToken gc = interpretGameResult resultToken <$> interpretMove moveN color moveToken gc

-- We currently don't support partial games, i.e. games where the first move in the text isn't move 1.
interpretGame :: GameToken -> AllHeaderData -> Either PgnError GameContext
interpretGame EmptyGame hd = return (startGame startPosition hd)
interpretGame (Game moveNumberToken) hd = interpretMoveNumber moveNumberToken (startGame startPosition hd)

createMoveToken :: AnnotatedPgnMove -> MoveToken
createMoveToken (pgnMove, Nothing) = UnannotatedMove pgnMove
createMoveToken (pgnMove, Just ann) = AnnotatedMove pgnMove (Annotation ann)

createBlackMoveToken :: Int -> Result -> AnnotatedPgnMove -> [AnnotatedPgnMove] -> BlackMoveToken
createBlackMoveToken _ res mv [] = FinalBlackMove (createMoveToken mv) (GameResult res)
createBlackMoveToken moveN res mv xs = BlackMove (createMoveToken mv) (createMoveNumberToken (moveN+1) res xs)

createWhiteMoveToken :: Int -> Result -> AnnotatedPgnMove -> [AnnotatedPgnMove] -> WhiteMoveToken
createWhiteMoveToken _ res mv [] = FinalWhiteMove (createMoveToken mv) (GameResult res)
createWhiteMoveToken moveN res mv (y:ys) = WhiteMove (createMoveToken mv) (createBlackMoveToken moveN res y ys)

createMoveNumberToken :: Int -> Result -> [AnnotatedPgnMove] -> MoveNumberToken
createMoveNumberToken moveN res [] = FinalMoveNumber (MN moveN) (GameResult res)
createMoveNumberToken moveN res (x:xs) = MoveNumber (MN moveN) (createWhiteMoveToken moveN res x xs)

createGameToken :: Result -> [AnnotatedPgnMove] -> GameToken
createGameToken _ [] = EmptyGame
createGameToken res xs = Game (createMoveNumberToken 1 res xs)

createPgnMove :: PositionContext -> UnderspecifiedMove -> PgnMove
createPgnMove _ (CastleMove direction _) = PgnCastleMove direction
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

createAnnotatedPgnMove :: PositionContext -> UnderspecifiedMove -> AnnotatedPgnMove
createAnnotatedPgnMove _ (CastleMove direction mAnnotation) = (PgnCastleMove direction, fmap mapMoveAnnotation mAnnotation)
createAnnotatedPgnMove nextPosition unspecMove = (pgnMove, mAnnotation) where
    pgnMove = createPgnMove nextPosition unspecMove
    mAnnotation = fmap mapMoveAnnotation (knownMoveAnnotation unspecMove)

createPgnMoves :: GameContext -> Either MoveError [AnnotatedPgnMove]
createPgnMoves gc
    | null (positions gc) = return []
    | otherwise = do
        unspecMoves <- createMinimallySpecifiedMoves gc
        -- We skip the first position because createPgnMove needs each move paired with the _next_ position,
        -- and the first position in the position list always precedes the first move in the move list.
        -- The call to tail should be safe because the list of positions should never be empty, but
        -- famous last words and all that.
        return $ zipWith createAnnotatedPgnMove (tail (positions gc)) unspecMoves

-- The PGN spec says that exported PGN strings should cut off the lines in the movetext at 80 characters.
writeGameToken :: GameToken -> String
writeGameToken = wrapLines 80 . unwords . map show . gameItems where

writeExtraHeaders :: [ExtraHeader] -> ShowS
writeExtraHeaders [] = showString ""
writeExtraHeaders (x:xs) = shows x . showString "\n" . writeExtraHeaders xs

writeAllHeaderData :: AllHeaderData -> ShowS
writeAllHeaderData (hd, extra) = shows hd . showString "\n" . writeExtraHeaders extra

writeGame :: AllHeaderData -> GameToken -> String
writeGame hdata gameToken = writeAllHeaderData hdata . showString "\n" $ writeGameToken gameToken

createGame :: GameContext -> Either MoveError GameToken
createGame gc = createGameToken (gameResult gc) <$> createPgnMoves gc

-- |Parses a PGN string and creates a GameContext,
--  or returns a PgnError if the PGN is syntactically or semantically invalid.
readPgn :: String -> Either PgnError GameContext
readPgn pgnString = do
    (headerTokens, gameToken) <- mapSyntaxError (parse pgn "ReadPgn" pgnString)
    hdata <- interpretHeaders headerTokens
    interpretGame gameToken hdata

-- |Writes a PGN string based on a GameContext.
--  This function may fail because it's possible for the GameContext to be invalid.
writePgn :: GameContext -> Either MoveError String
writePgn gc = writeGame (allHeaderData gc) <$> createGame gc
