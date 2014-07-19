{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Openings where

import ChessTestUtils

start = game ([] :: [String])
berlin = ruyLopez `addMove` "Nf6"
caroKann = game ["e4", "c6"]
fastestPossibleStalemate = game ["c4", "h5", "h4", "a5", "Qa4", "Ra6", "Qxa5", "Rah6", "Qxc7", "f6", "Qxd7", "Kf7", "Qxb7", "Qxd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"]
foolsMate = game ["f3", "e6", "g4", "Qh4#"]
french = game ["e4", "e6"]
kingPawn = game ["e4"]
najdorf = sicilian `addMoves` ["Nf3", "d6", "d4", "cxd4", "Nxd4", "Nf6", "Nc3", "a6"]
nimzo = game ["d4", "Nf6", "c4", "e6", "Nc3", "Bb4"]
queenPawn = game ["d4"]
reti = game ["Nf3"]
ruyLopez = game ["e4", "e5", "Nf3", "Nc6", "Bb5"]
scandinavian = game ["e4", "d5"]
scholarsMate = game ["e4", "e5", "Qh5", "Nc6", "Bc4", "Nf6", "Qxf7#"]
sicilian = game ["e4", "c5"]
