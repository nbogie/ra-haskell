Beginner haskell implementation of Reiner Knizia's game, Ra.
http://boardgamegeek.com/boardgame/12/ra

This is just on github as a backup of a learning project.  It is NOT a good example of haskell to learn from.

No AI, no GUI (just a messy text interface).  Supports from 2 to 5 players.

The main issue with this current implementation is the incomplete separation of IO from game logic.

To run the game with n players, invoke with -p n  E.g. A 3 player game can be started as follows:
    runhaskell GUI.hs -p 3 

