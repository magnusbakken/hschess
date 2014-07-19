{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Openings (
    kingPawn,
    queenPawn,
    french,
    sicilian,
    ruyLopez,
    najdorf
) where

import ChessTestUtils (forceGame, forceGameFrom)

kingPawn = forceGame ["e4"]
queenPawn = forceGame ["d4"]
french = forceGame ["e4", "e6"]
sicilian = forceGame ["e4", "c5"]
ruyLopez = forceGame ["e4", "e5", "Nf3", "Nc6", "Bb5"]
najdorf = forceGameFrom sicilian ["Nf3", "d6", "d4", "cxd4", "Nxd4", "Nf6", "Nc3", "a6"]
