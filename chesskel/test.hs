module Chesskel.Test (
    testCastling,
    testEnPassant,
    testCheckmate,
    testStalemate,
    testPromotion
) where

import Control.Applicative

-- Test-related functions
parseFile :: Char -> Maybe File
parseFile 'a' = Just FileA
parseFile 'b' = Just FileB
parseFile 'c' = Just FileC
parseFile 'd' = Just FileD
parseFile 'e' = Just FileE
parseFile 'f' = Just FileF
parseFile 'g' = Just FileG
parseFile 'h' = Just FileH
parseFile _ = Nothing

parseRank :: Char -> Maybe Rank
parseRank '1' = Just Rank1
parseRank '2' = Just Rank2
parseRank '3' = Just Rank3
parseRank '4' = Just Rank4
parseRank '5' = Just Rank5
parseRank '6' = Just Rank6
parseRank '7' = Just Rank7
parseRank '8' = Just Rank8
parseRank _ = Nothing

parseCell :: Char -> Char -> Maybe Cell
parseCell file rank = createCell <$> parseFile file <*> parseRank rank

readCell :: String -> Maybe Cell
readCell (file:rank:[]) = parseCell file rank
readCell _ = Nothing

parsePiece :: Char -> Maybe Piece
parsePiece 'P' = Just (Pawn, White)
parsePiece 'N' = Just (Knight, White)
parsePiece 'B' = Just (Bishop, White)
parsePiece 'R' = Just (Rook, White)
parsePiece 'Q' = Just (Queen, White)
parsePiece 'K' = Just (King, White)
parsePiece 'p' = Just (Pawn, Black)
parsePiece 'n' = Just (Knight, Black)
parsePiece 'b' = Just (Bishop, Black)
parsePiece 'r' = Just (Rook, Black)
parsePiece 'q' = Just (Queen, Black)
parsePiece 'k' = Just (King, Black)
parsePiece _ = Nothing

readPiece :: String -> Maybe Piece
readPiece (c:[]) = parsePiece c
readPiece _ = Nothing

readLongNotationMove :: String -> Maybe (Move, Maybe PromotionTarget)
readLongNotationMove moveStr
    | length moveStr == 5 = fmap (\move -> (move, Nothing)) (readMove firstFive)
    | length moveStr == 6 = (\move pt -> (move, Just pt)) <$> readMove firstFive <*> readPromotionTarget sixth
    | otherwise = Nothing where
        firstFive = take 5 moveStr
        sixth = moveStr !! 5

readMove :: String -> Maybe Move
readMove (fromFile:fromRank:'-':toFile:toRank:[]) =
    createMove <$> parseCell fromFile fromRank <*> parseCell toFile toRank
readMove _ = Nothing

readPromotionTarget :: Char -> Maybe PromotionTarget
readPromotionTarget 'n' = Just PKnight
readPromotionTarget 'b' = Just PBishop
readPromotionTarget 'r' = Just PRook
readPromotionTarget 'q' = Just PQueen
readPromotionTarget _ = Nothing

testMove :: String -> PositionContext -> PositionContext
testMove moveStr pc = fromJust $ do
    (move, mpt) <- readLongNotationMove moveStr
    case makeMove pc move mpt of
        Left e -> error $ moveStr ++ ": " ++ show e
        Right mc -> return mc

testHasLegalMove :: String -> PositionContext -> Bool
testHasLegalMove moveStr pc = maybe False (uncurry $ isLegalMove pc) (readLongNotationMove moveStr)

testMoves :: PositionContext -> [String] -> PositionContext
testMoves = foldl (flip testMove)

testFromStart :: [String] -> PositionContext
testFromStart = testMoves startPosition

testHasPiece :: Char -> String -> PositionContext -> Bool
testHasPiece pieceStr cellStr pc = fromMaybe False $ do
    expectedPiece <- parsePiece pieceStr
    cell <- readCell cellStr
    actualPiece <- getSquare (position pc) cell
    return $ expectedPiece == actualPiece

-- Tests:
testCastling = testHasLegalMove "e1-g1" $ testFromStart ["g1-f3", "g8-f6", "g2-g3", "g7-g6", "f1-g2", "f8-g7"]
testEnPassant = testHasLegalMove "e5-d6" $ testFromStart ["e2-e4", "g8-f6", "e4-e5", "d7-d5"]
testCheckmate = isCheckmate $ testFromStart ["f2-f3", "e7-e6", "g2-g4", "d8-h4"]
testStalemate = isStalemate $ testFromStart ["c2-c4", "h7-h5", "h2-h4", "a7-a5", "d1-a4", "a8-a6", "a4-a5", "a6-h6", "a5-c7", "f7-f6", "c7-d7", "e8-f7", "d7-b7", "d8-d3", "b7-b8", "d3-h7", "b8-c8", "f7-g6", "c8-e6"]
testPromotion = testHasPiece 'Q' "a8" $ testFromStart ["a2-a4", "b7-b5", "a4-b5", "a7-a6", "b5-a6", "b8-c6", "a6-a7", "a8-b8", "a7-a8q"]
