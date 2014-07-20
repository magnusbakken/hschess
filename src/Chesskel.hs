{-|
Module      : Chesskel
Description : A chess API.
Copyright   : (c) Magnus Grindal Bakken, 2014
License     : MIT
Maintainer  : magnusbakken@gmail.com
Stability   : experimental
Portability : POSIX

This module combines the modules for positions ("Chesskel.Board"), moves
("Chesskel.Movement") and games ("Chesskel.Gameplay"). If you only need parts
of the functionality, consider importing those modules separately as needed
instead.
-}
module Chesskel (
    -- |The "Chesskel.Board" module contains basic data types and functions for
    --  positions, cells, pieces, etc.
    module Chesskel.Board,
    
    -- |The "Chesskel.Movement" module contains data types and functions for
    --  single moves.
    module Chesskel.Movement,
    
    -- |The "Chesskel.Gameplay" module contains data types and functions for
    --  entire games.
    module Chesskel.Gameplay,
) where

import Chesskel.Board
import Chesskel.Movement
import Chesskel.Gameplay
