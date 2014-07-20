{-|
Module      : Chesskel.Utils
Description : Internal generic utility functions for the Chesskel API.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This internal module contains utility functions that are fairly generic but
that the author hasn't found in any readily available packages.
-}
module Chesskel.Utils (
    wrapLines,
    maybeToEither,
    isRight,
    interpolate,
    compress
) where

import Data.Char (isSpace)
import Data.List (group)

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
    | any isSpace beforeMax  = beforeSpace : wrapLine maxLen (afterSpace ++ afterMax)
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

-- |Gets interpolated items strictly between two bounds. Also works for backwards ranges.
--
--  > interpolate 1 3 == [2]
--  > interpolate 4 1 == [3,2]
--  > interpolate 1 2 == []
interpolate :: (Enum n, Ord n) => n -> n -> [n]
interpolate from to
    | from == to = []
    | from > to = reverse (interpolate to from)
    | otherwise = [succ from..pred to]

-- |Removes consecutive duplicate elements from a list.
compress :: Eq a => [a] -> [a]
compress = map head . group
