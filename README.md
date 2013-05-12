What is it?
===========

A beginner haskell implementation of Reiner Knizia's game, Ra.
(http://boardgamegeek.com/boardgame/12/ra)

This is just on github as a backup of a learning project.  It is NOT a good example of haskell to learn from.  For that, I would recommend Pedro Vasconcelos's implementation of Lost Cities: http://www.ncc.up.pt/~pbv/stuff/lostcities/ (another Knizia game).

This implementation supports all rules as stated here, with play for 2 to 5 players: 
http://www.riograndegames.com/uploads/Game/Game_266_gameRules.pdf

It supports some unofficial rules (for example, you can take a Disaster tile with a God tile (You still need to resolve it)).

Limitations:
============

Huge aesthetic issues in both text and graphical interfaces.

Neither AI nor network play!

Scoring is computed but not shown.

Usage:
======

Invoking "cabal build" will create two executables: "ra-text" and "ra-gui".  Both take the same command-line arguments.

To run the game with n players, invoke with -p n  E.g. A 3 player game can be started as follows:

    ra-gui -p 3 

An event-heavy tileset, meant for development only, can be enabled with the -d switch, e.g. 

    ra-gui -p 4 -d
