# hschess
A Haskell chess implementation, originally written in 2014. Mostly intended as a practice project for learning intermediate Haskell patterns, but I tried to put some effort into the structure and comments.

This is just an API for chess moves. There's no UI.

Probably the most interesting part of the project is the QuickCheck test suite, which generates random games and verifies various chess invariants.
