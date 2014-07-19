{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Openings where

import ChessTestUtils

berlin = ruyLopez `addMove` "Nf6"
caroKann = game ["e4", "c6"]
foolsMate = game ["f3", "e6", "g4", "Qh4#"]
french = game ["e4", "e6"]
kingPawn = game ["e4"]
najdorf = sicilian `addMoves` ["Nf3", "d6", "d4", "cxd4", "Nxd4", "Nf6", "Nc3", "a6"]
nimzo = game ["d4", "Nf6", "c4", "e6", "Nc3", "Bb4"]
queenPawn = game ["d4"]
reti = game ["Nf3"]
ruyLopez = game ["e4", "e5", "Nf3", "Nc6", "Bb5"]
sicilian = game ["e4", "c5"]
