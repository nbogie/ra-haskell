What is it?
===========

A beginner haskell implementation of Reiner Knizia's game, Ra.
(http://boardgamegeek.com/boardgame/12/ra)

This is just on github as a backup of a learning project.  It is NOT a good example of haskell to learn from.  For that, I would recommend Pedro Vasconcelos's implementation of Lost Cities: http://www.ncc.up.pt/~pbv/stuff/lostcities/

Supports all rules as stated here, with play for 2 to 5 players: 
http://www.riograndegames.com/uploads/Game/Game_266_gameRules.pdf

It supports some undocumented rules (for example, you can take a Disaster tile with a God tile (You still need to resolve it)).

Limitations:
============

No AI
No GUI
Messy, barely useable text interface

From a code-standpoint, the main issue with this current implementation is the incomplete separation of IO from game logic.

Usage:
======

To run the game with n players, invoke with -p n  E.g. A 3 player game can be started as follows:
    runhaskell GUI.hs -p 3 

An event-heavy tileset, meant for development not balanced play, can be enabled with the -d switch, e.g. 
    runhaskell GUI.hs -p 4 -d
