-- We want the deck to be list of type a, but a player hand to be list of some subclass b of those.  How best to structure this?

data Tile = Storeable StoreableTile 
          | NonStoreable NonstoreableTile deriving (Show, Eq, Ord)

-- A non-solution follows, working only where we have only two types of tile with no overlap.
-- This was implemented in commit 77c16fdbb6627156f7a3f78b1c37c9730d7c4e79 :
-- https://github.com/nbogie/ra-haskell/commit/77c16fdbb6627156f7a3f78b1c37c9730d7c4e79
--
-- TODO: problem, we have more than two categories of tile, namely
-- 
-- * _/ Storeable    - everything that can be held in a player hand. Not Ra or Disaster.
-- *  \ NonStoreable - Ra, Disasters
-- * Auctionable     - everything except Ra.
-- * Goddable        - everything except Ra and God.
--
-- Some example problem tiles:
-- Ra:       NonStoreable
-- Disaster: NonStoreable, Goddable, Auctionable
-- God:         Storeable, Goddable, Auctionable
-- All Others:  Storeable, Goddable, Auctionable
data NonstoreableTile = Ra | Disaster
                      deriving (Show, Eq, Enum, Ord, Bounded)
data StoreableTile = Pharaoh | Gold 
                   deriving (Show, Eq, Enum, Ord, Bounded)

allTiles :: [Tile]
allTiles = map Storeable [minBound .. maxBound] 
         ++ map NonStoreable [minBound .. maxBound]

data Player = Player { name :: String, 
                       hand :: [StoreableTile] } deriving (Show)
data Game = Game { auctionBlock :: [Tile] } deriving (Show)
 -- we'd like to have this be auctionBlock :: [AuctionableTile]

splitHandAndEvents :: [Tile] -> ([StoreableTile], [NonstoreableTile])
splitHandAndEvents ts = (sts, nsts)
  where 
    sts = [t | Storeable t <- ts]
    nsts = [t | NonStoreable t <- ts]

main = do
  let ts = allTiles
  let (hand, events) = splitHandAndEvents ts
  print (hand, events)
