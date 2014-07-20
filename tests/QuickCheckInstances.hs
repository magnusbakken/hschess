{-# OPTIONS_GHC -fno-warn-orphans #-}
module QuickCheckInstances where

import Chesskel.Board hiding (whiteKing, blackKing)
import Chesskel.Gameplay
import Chesskel.Movement
import Control.Applicative
import Test.QuickCheck

instance Arbitrary File where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Rank where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Cell where
    arbitrary = curry Cell <$> arbitrary <*> arbitrary

instance Arbitrary Chessman where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Color where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Move where
    arbitrary = curry Move <$> arbitrary <*> arbitrary

arbitraryMove :: PositionContext -> Gen (Maybe MoveContext)
arbitraryMove pc = fst <$> randomMove pc

-- |A generator that returns an arbitrary move for an arbitrary position,
--  which has been arrived at after a given number of moves.
arbitraryMoveForPosition :: Int -> Gen (MoveContext, PositionContext)
arbitraryMoveForPosition n = do
    pc <- arbitraryPositionContext n
    case findAllLegalMoves pc of
        -- If we're unlucky enough to get a position where there are no moves
        -- we'll just try again. This should be extremely rare.
        [] -> arbitraryMoveForPosition n
        moveList -> flip (,) pc <$> elements moveList

arbitraryGameContext :: Int -> Gen GameContext
arbitraryGameContext size = go size startStandardGame where
    go 0 gc = return gc
    go n gc = randomMoveForGame gc >>= go (pred n)

arbitraryPositionContext :: Int -> Gen PositionContext
arbitraryPositionContext = fmap currentPosition . arbitraryGameContext
