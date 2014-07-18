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
data MoveToken = AnnotatedMove UnderspecifiedMove AnnotationToken | UnannotatedMove UnderspecifiedMove deriving (Eq)
data AnnotationToken = Annotation MoveAnnotation deriving (Eq)
data GameResultToken = GameResult Result deriving (Eq)

newtype MoveNum = MN Int deriving (Eq, Ord, Bounded)

data GameItem = MoveNumItem MoveNum |
                MoveItem UnderspecifiedMove |
                AnnotatedMoveItem UnderspecifiedMove MoveAnnotation |
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
    gameItem (AnnotatedMove mv (Annotation ann)) = AnnotatedMoveItem mv ann
    gameItem (UnannotatedMove mv) = MoveItem mv

instance LeafToken MoveNum where
    gameItem = MoveNumItem

instance LeafToken GameResultToken where
    gameItem (GameResult res) = ResultItem res

instance Show MoveNum where
    show (MN n) = show n ++ "."

instance Show GameItem where
    show (MoveNumItem moveNum) = show moveNum
    show (MoveItem unspecMove) = show unspecMove
    show (AnnotatedMoveItem unspecMove ann) = show unspecMove ++ " " ++ show ann
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

annotation = do
    char '{'
    ann <- many (noneOf "{}")
    char '}'
    many1 space
    return $ MA (T.replace (T.pack "\n") (T.pack " ") (T.pack ann))

moveAndAnnotation = do
    mv <- sanMove
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

mapGameplayError :: Int -> Color -> MinimalMove -> Either MoveError a -> Either PgnError a
mapGameplayError _ _ _ (Right a) = Right a
mapGameplayError moveN color (unspecMove, _) (Left e) =
    let dots = if color == White then "." else "..."
        moveStr = show moveN ++ dots ++ " " ++ show unspecMove in
    Left $ InvalidMoveError $ "Invalid move: " ++ moveStr ++ " (" ++ show e ++ ")"

interpretMinimalMove :: Int -> Color -> MinimalMove -> GameContext -> Either PgnError GameContext
interpretMinimalMove moveN color miniMove gc = mapGameplayError moveN color miniMove $ playMinimalMove miniMove gc

interpretMove :: Int -> Color -> MoveToken -> GameContext -> Either PgnError GameContext
interpretMove moveN color (AnnotatedMove unspecMove (Annotation ann)) gc = interpretMinimalMove moveN color (unspecMove, Just ann) gc
interpretMove moveN color (UnannotatedMove unspecMove) gc = interpretMinimalMove moveN color (unspecMove, Nothing) gc

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

createMoveToken :: MinimalMove -> MoveToken
createMoveToken (unspecMove, Nothing) = UnannotatedMove unspecMove
createMoveToken (unspecMove, Just ann) = AnnotatedMove unspecMove (Annotation ann)

createBlackMoveToken :: Int -> Result -> MinimalMove -> [MinimalMove] -> BlackMoveToken
createBlackMoveToken _ res miniMove [] = FinalBlackMove (createMoveToken miniMove) (GameResult res)
createBlackMoveToken moveN res miniMove xs = BlackMove (createMoveToken miniMove) (createMoveNumberToken (moveN+1) res xs)

createWhiteMoveToken :: Int -> Result -> MinimalMove -> [MinimalMove] -> WhiteMoveToken
createWhiteMoveToken _ res mv [] = FinalWhiteMove (createMoveToken mv) (GameResult res)
createWhiteMoveToken moveN res mv (y:ys) = WhiteMove (createMoveToken mv) (createBlackMoveToken moveN res y ys)

createMoveNumberToken :: Int -> Result -> [MinimalMove] -> MoveNumberToken
createMoveNumberToken moveN res [] = FinalMoveNumber (MN moveN) (GameResult res)
createMoveNumberToken moveN res (x:xs) = MoveNumber (MN moveN) (createWhiteMoveToken moveN res x xs)

createGameToken :: Result -> [MinimalMove] -> GameToken
createGameToken _ [] = EmptyGame
createGameToken res xs = Game (createMoveNumberToken 1 res xs)

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
createGame gc = createGameToken (gameResult gc) <$> createMinimallySpecifiedMoves gc

-- |Parses a PGN string and creates a GameContext,
--  or returns a PgnError if the PGN is syntactically or semantically invalid.
readPgn :: String -> Either PgnError GameContext
readPgn pgnString = do
    (headerTokens, gameToken) <- mapSyntaxError (parse pgn "ReadPgn" pgnString)
    hdata <- interpretHeaders headerTokens
    interpretGame gameToken hdata

-- |Writes a PGN string based on a GameContext.
--
--  This function may fail because it's possible for the GameContext to be invalid.
writePgn :: GameContext -> Either MoveError String
writePgn gc = writeGame (allHeaderData gc) <$> createGame gc
