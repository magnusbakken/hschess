{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Openings (
    kingPawn,
    queenPawn,
    french,
    sicilian,
    ruyLopez,
    najdorf
) where

import ChessTestUtils

kingPawn = forcePosition ["e4"]
queenPawn = forcePosition ["d4"]
french = forcePosition ["e4", "e6"]
sicilian = forcePosition ["e4", "c5"]
ruyLopez = forcePosition ["e4", "e5", "Nf3", "Nc6", "Bb5"]
najdorf = forcePosition ["e4", "c5", "Nf3", "d6", "d4", "cxd4", "Nxd4", "Nf6", "Nc3", "a6"]
