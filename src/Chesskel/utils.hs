module Chesskel.Utils (
    wrapLines,
    maybeToEither,
    isRight
) where

import Data.Char (isSpace)

trim :: String -> String
trim = trimAndReverse . trimAndReverse
    where trimAndReverse = reverse . dropWhile isSpace

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
    where (after, before) = break f $ reverse xs

-- Thanks to More Indirection for this word wrapping algorithm.
-- http://moreindirection.blogspot.no/2010/08/blog-post.html
wrapLine :: Int -> String -> [String]
wrapLine maxLen line 
    | length line <= maxLen  = [line]
    | any isSpace beforeMax  = beforeSpace : (wrapLine maxLen $ afterSpace ++ afterMax)
    | otherwise              = beforeMax : wrapLine maxLen afterMax where
        (beforeMax, afterMax) = splitAt maxLen line
        (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax

wrapLines :: Int -> String -> String
wrapLines maxLen = unlines . concatMap (map trim . wrapLine maxLen) . lines

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

isRight :: Either e a -> Bool -- Added to Data.Either in recent GHC version.
isRight (Right _) = True
isRight (Left _) = False
