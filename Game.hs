module Game where

import Control.Arrow ((&&&))
import Data.List (nub, sort, group, (\\), foldl', find)
import Data.Maybe (isNothing)
import Debug.Trace (trace, traceShow)
import Prelude hiding (pi)
import Test.HUnit
import qualified Data.Map as M

import Shuffle

class (Show a) => ToChar a where

  toChar :: a -> Char

  -- freebies
  toCharAndShow :: a -> String
  toCharAndShow t = paren c ++ " " ++ show t
     where 
       c = toChar t
       paren x = "(" ++ [x] ++ ")"

  showName :: a -> String


data Tile = Storeable StoreableTile
          | NonStoreable NonStoreableTile deriving (Show, Eq, Ord)

data NonStoreableTile = Ra | Disaster DisasterType deriving (Eq, Show, Ord)

data StoreableTile = 
            Pharaoh 
          | God 
          | Gold 
          | Nile 
          | Flood 
          | Monument MonumentType
          | Civilization CivilizationType
          deriving (Eq, Show, Ord)

{--
  We ask isTempTile only of a Tile Storeable, and Ra and Disaster tiles are not storeable.  
  However, Disaster tiles may be taken using a god, that is not storeability but takeability.
  We want to be able to store a mix of such tiles in the deck, such that a Ra and a God and a Flood could all be in the same list of tiles - does this preclude type classes?
--}
isPermanentTile ::  StoreableTile -> Bool
isPermanentTile = not . isTempTile

isTempTile :: StoreableTile -> Bool
isTempTile t = case t of
  Pharaoh        -> False
  God            -> True
  Gold           -> True
  Nile           -> False
  Flood          -> True
  Monument _     -> False
  Civilization _ -> True

isGoddable :: Tile -> Bool
isGoddable (Storeable God) = False
-- TODesign: this question should never be asked.  we ask isGoddable of an Tile Auctionable
isGoddable (NonStoreable Ra)  = False
isGoddable _   = True

instance ToChar Tile where
   toChar (Storeable t)    = toChar t
   toChar (NonStoreable t) = toChar t
   showName (Storeable t) = showName t
   showName (NonStoreable t) = showName t


instance ToChar NonStoreableTile where
   toChar Ra            = 'R'
   toChar (Disaster dt) = toChar dt
   showName Ra = "Ra"
   showName (Disaster dt) = show dt

instance ToChar StoreableTile where
   toChar t = case t of
     Pharaoh         -> 'P'
     God             -> 'G'
     Gold            -> '$'
     Nile            -> '-'
     Flood           -> '~'
     Monument mt     -> toChar mt
     Civilization ct -> toChar ct

   showName = show

instance ToChar MonumentType where
   toChar t = case t of
      Fortress    -> '1'
      Obelisk     -> '2'
      Palace      -> '3'
      Pyramid     -> '4'
      Sphinx      -> '5'
      Statues     -> '6'
      StepPyramid -> '7'
      Temple      -> '8'
   showName = show
  
data MonumentType = Fortress 
                  | Obelisk 
                  | Palace 
                  | Pyramid 
                  | Sphinx 
                  | Statues 
                  | StepPyramid 
                  | Temple 
                  deriving (Eq, Show, Ord, Bounded, Enum)

data DisasterType = Funeral 
                  | Unrest 
                  | Drought 
                  | Earthquake 
                  deriving (Eq, Show, Ord, Bounded, Enum)


instance ToChar DisasterType where
   toChar t = case t of
     Funeral    -> 'f'
     Unrest     -> 'u'
     Drought    -> '_'
     Earthquake -> '0'
   showName = show


data CivilizationType = Art 
                      | Agriculture 
                      | Religion 
                      | Astronomy 
                      | Writing 
                      deriving (Eq, Show, Ord, Bounded, Enum)
instance ToChar CivilizationType where
  toChar t = case t of
      Art         -> '@'
      Agriculture -> 'v'
      Religion    -> '^'
      Astronomy   -> '*'
      Writing     -> '&'
  showName = show

allTilesTweaked :: [Tile]
allTilesTweaked = map Storeable allStoreableTilesTweaked ++ map NonStoreable allNonStoreableTilesTweaked

allTiles :: [Tile]
allTiles = map Storeable allStoreableTiles ++ map NonStoreable allNonStoreableTiles

allNonStoreableTiles :: [NonStoreableTile]
allNonStoreableTiles = 
   replicate 30 Ra      ++ 
   replicate 2 funeral  ++ 
   replicate 2 drought  ++ 
   replicate 4 unrest   ++ 
   replicate 2 earthquake

allStoreableTiles :: [StoreableTile]
allStoreableTiles = 
           replicate 8 God      ++ 
           replicate 25 Pharaoh ++ 
           replicate 25 Nile    ++ 
           replicate 12 Flood   ++ 
           allCivilizations     ++ 
           replicate 5 Gold     ++ 
           allMonuments

-------------------------------------------------
-- Tweaked tile set for dev
-------------------------------------------------
allNonStoreableTilesTweaked :: [NonStoreableTile]
allNonStoreableTilesTweaked = 
   replicate 10 Ra      ++ 
   replicate 4 funeral  ++ 
   replicate 4 drought  ++ 
   replicate 6 unrest   ++ 
   replicate 4 earthquake

allStoreableTilesTweaked :: [StoreableTile]
allStoreableTilesTweaked = 
           replicate 20 God      ++ 
           replicate 15 Pharaoh ++ 
           replicate 10 Nile    ++ 
           replicate 12 Flood   ++ 
           allCivilizationsTweaked     ++ 
           replicate 5 Gold     ++ 
           allMonumentsTweaked

allMonumentTypes ::  [MonumentType]
allMonumentTypes     = [minBound .. maxBound]
allCivilizationTypes ::  [CivilizationType]
allCivilizationTypes = [minBound .. maxBound]

allMonuments :: [StoreableTile]
allMonuments     = concatMap (replicate 5 . Monument) allMonumentTypes
allCivilizations :: [StoreableTile]
allCivilizations = concatMap (replicate 5 . Civilization) allCivilizationTypes
allCivilizationsTweaked ::  [StoreableTile]
allCivilizationsTweaked = concatMap (replicate 10 . Civilization) allCivilizationTypes

allMonumentsTweaked  :: [StoreableTile]
allMonumentsTweaked = concatMap (replicate 10 . Monument) allMonumentTypes
funeral :: NonStoreableTile
funeral = Disaster Funeral
drought ::  NonStoreableTile
drought = Disaster Drought
unrest ::  NonStoreableTile
unrest = Disaster Unrest
earthquake ::  NonStoreableTile
earthquake = Disaster Earthquake

newtype Sun = Sun { sunValue :: Int } deriving (Eq, Ord)
instance Show Sun where
  show (Sun i) = show i

type Deck = [Tile]
type Block = [Tile]

blockIsEmpty :: Board -> Bool
blockIsEmpty = null . block
blockIsNotEmpty :: Board -> Bool
blockIsNotEmpty = not . blockIsEmpty
blockFull :: Board -> Bool
blockFull = (>=blockMax) . length . block

blockToSummaryString :: Block -> String
blockToSummaryString  = unlines . map show . sort . map (head &&& length) . group . sort

blockToString :: Block -> String
blockToString = padWith '.' blockMax . map toChar
playerToString ::(PlayerNum, Player) -> String
playerToString (i, p) = "Player: " ++ show i ++ ": (" ++ show (score p) ++ "): " ++ show (sunsSorted p) ++ ", " ++ ts
  where ts            = tilesToString . tiles $ p
        tilesToString = map toChar . concat . group . sort 

raTrackToString ::  Board -> String
raTrackToString b = padWith '.' (raCountMax b) $ replicate (raCount b) 'R'

padWith ::  a -> Int -> [a] -> [a]
padWith c n s | length s >= n = s
              | otherwise     = replicate padding c ++ s
   where padding = n - length s


-- given a number of players, report the max number of spaces in ra track
raCountMax :: Board -> Int
raCountMax = raCountMaxFor . numPlayers

raCountMaxFor :: Int -> Int
raCountMaxFor 2 = 6
raCountMaxFor 3 = 8
raCountMaxFor 4 = 9
raCountMaxFor 5 = 10
raCountMaxFor other = error $ "BUG in raCountMaxFor: illegal number of players: " ++ show other

boardToString :: Board -> String
boardToString b = unlines $ [ 
                            show $ epoch b
                          , "Block:    " ++ blockToString (block b)
                          , "Ra Track: " ++ raTrackToString b
                          , "Sun:      " ++ show (boardSun b)
                          ] ++ playerDisplay
   where 
     playerDisplay :: [String]
     playerDisplay = map playerToString (playersFromCurrent b)

playersFromCurrent :: Board -> [(PlayerNum, Player)]
playersFromCurrent b = map (\i -> (i, players b M.! i)) (take lim $ playerCycle b)

  where lim = numPlayers b
data Player = Player { suns :: ([Sun], [Sun])
                     , tiles :: [StoreableTile]
                     , score :: Int
                     } deriving (Show, Eq)

sunsSorted :: Player -> ([Sun], [Sun])
sunsSorted p = let (ups, downs) = suns p 
               in (sort ups, sort downs)

data Board = Board { raCount :: Int
                   , gameMode :: GameMode
                   , block :: Block
                   , boardSun :: Sun
                   , epoch :: Epoch
                   , deck :: Deck
                   , players :: M.Map PlayerNum Player
                   , currentPlayerId :: Int
                   } deriving (Show, Eq)

playerIds ::  Board -> [Int]
playerIds = M.keys . players

newtype Epoch = Epoch { epochInt :: Int } deriving (Eq)
instance Show Epoch where
  show (Epoch i) = "Epoch " ++ show i

numPlayers :: Board -> Int
numPlayers = length . M.keys . players

startingSuns ::  [[[Sun]]]
startingSuns = fmap (fmap (fmap Sun)) 
               [ [[9,  6, 5, 2], [8, 7, 4, 3]] -- 2 players
               , [[13, 8, 5, 2], [12, 9, 6, 3], [11, 10, 7, 4]] --3 players
               , [[13, 6, 2], [12, 7, 3], [11, 8, 4], [10, 9, 5]] --4 players
               , [[16, 7, 2], [15, 8, 3], [14, 9, 4], [13, 10, 5], [12, 11, 6]] -- 5 players
               ]

initPlayers :: Int -> [Player]
initPlayers n = map (\ss -> Player (ss, []) [] 10) sunSets
  where sunSets = head $ filter ((==n) . length) startingSuns 

initBoard :: Int -> [Tile] -> Board
initBoard nPlayers ts = Board 
             { raCount = 0
             , gameMode = StartTurn
             , block = []
             , boardSun = Sun 1
             , epoch = Epoch 1
             , deck = ts
             , players = M.fromList $ zip (playerCycleFromTo 0 nPlayers) (initPlayers nPlayers) 
             , currentPlayerId = 0
             } 

playerCycleFrom :: PlayerNum -> Board -> [PlayerNum]
playerCycleFrom pi b = map (`mod` mx) [pi ..] 
  where mx = numPlayers b

playerCycleFromTo :: PlayerNum -> PlayerNum -> [PlayerNum]
playerCycleFromTo pi mx = map (`mod` mx) [pi ..]

playerCycle ::  Board -> [PlayerNum]
playerCycle b = playerCycleFrom (currentPlayerId b) b

active ::  Board -> Player
active board = players board M.! currentPlayerId board


currentOrAuctionCurrentPlayerNum :: Board -> PlayerNum
currentOrAuctionCurrentPlayerNum board = 
   case gameMode board of
       (InAuction _reason (pi:_) _)                   -> pi
       (ResolveDisasters _ _ (AuctionDRContext pi _)) -> pi
       _                                              -> currentPlayerId board

currentOrAuctionCurrentPlayer :: Board -> Player
currentOrAuctionCurrentPlayer board = 
  players board M.! currentOrAuctionCurrentPlayerNum board

advancePlayer :: Board -> Board
advancePlayer b = 
  case nextPlayerM of
     Just pi  -> b { currentPlayerId = pi }
     Nothing -> error "BUG: advancePlayer called when no one left in play"
     -- TODO: even when no one left in play, player should still get advanced (at least for start of next epoch)
  where
        nextPlayerM = find (isStillInPlay b) nextPlayers
        nextPlayers = take (numPlayers b) (playerCycleFrom (1 + currentPlayerId b) b)

advanceEpoch :: Board -> Board
advanceEpoch b = b { epoch = adv (epoch b) } 
  where
   adv (Epoch 1) = Epoch 2
   adv (Epoch 2) = Epoch 3
   adv other = error $ "BUG: Asked to advanceEpoch for epoch other than 1 or 2: " ++ show other

removeTempTiles :: Player -> Player
removeTempTiles p = p { tiles = filter isPermanentTile $ tiles p }

scoreEpoch :: Board -> Board
scoreEpoch b = forAllPlayers (scoreEpochForPlayer isFinal pharCounts sunTotals) b
  where 
    pharCounts = map (length . filter (==Pharaoh) . tiles) $ M.elems $ players b
    sunTotals  = map (totalSunCount . suns) $ M.elems $ players b
    isFinal = epoch b == Epoch 3

-- Total values of all your suns, regardless of facing
-- Used in final scoring, and perhaps by AI.  
-- Keep impl out of scoring, as scoring doesn't want to know about the structure of the sun storeage
totalSunCount :: SunsUpDown -> Int
totalSunCount = sum . map sunValue . fst . turnSunsFaceUp

data ScoreReason = ScGods Int
                 | ScPharaohs Int (Int, ComparativeScoreReason)
                 | ScNiles Int (Int,Int)
                 | ScCivs Int Int
                 | ScGolds Int
                 | ScMonuments Int Int
                 | ScSuns Int (Int, ComparativeScoreReason)
                 deriving (Show, Eq)

scoreFrom :: ScoreReason -> Int
scoreFrom r = case r of
  ScGods i          -> i
  ScPharaohs i _    -> i
  ScNiles i _       -> i
  ScCivs i _        -> i
  ScGolds i         -> i
  ScMonuments x y   -> x + y
  ScSuns i _        -> i

explainScore :: ScoreReason -> String
explainScore r = case r of
  ScGods i                  -> sayPoints i ++ " for God tiles (2 points per God Tile)"
  ScGolds i                 -> sayPoints i ++ " for Golds (3 points per Gold)."
  ScPharaohs i (n,Max)      -> sayPoints i ++ " for having the highest number of Pharaohs " ++ paren (show n)
  ScPharaohs i (n,Min)      -> sayPoints i ++ " for having the least number of Pharaohs " ++ paren (show n)
  ScPharaohs i (n,AllEqual) -> sayPoints i ++ " as everyone had the same number of Pharaohs " ++ paren (show n)
  ScPharaohs i (n,Neutral)  -> sayPoints i ++ " for having neither the highest nor lowest number of Pharaohs " ++ paren (show n)
  ScSuns i (n,Max)          -> sayPoints i ++ " for having the highest total Sun points " ++ paren (show n)
  ScSuns i (n,Min)          -> sayPoints i ++ " for having the lowest total Sun points " ++ paren (show n)
  ScSuns i (n,Neutral)      -> sayPoints i ++ " for having neither highest nor lowest total Sun points " ++ paren (show n)
  ScSuns i (n,AllEqual)     -> sayPoints i ++ " as everyone had the same total Sun points " ++ paren (show n)
  ScNiles i (nnile, nflood) -> sayPoints i ++ " for "++ show nnile ++" Nile and "++ show nflood ++" Flood tiles."
                               ++ if nflood == 0 && nnile > 0 
                                  then "  (You needed at least one Flood tile to score points.)"
                                  else ""
  ScCivs i nDistinct        -> sayPoints i ++ " for " ++ show nDistinct ++ " distinct civilization tiles."
                               ++ if nDistinct < 3 then "  (You need at least 3 distinct to score points.)" else ""
  ScMonuments x y           -> sayPoints (scoreFrom r) ++ " total from Monuments "
                               ++ paren ( sayPoints x ++ " for distinct types, and " 
                                  ++ sayPoints y ++ " for duplicates.")
  where paren str = "(" ++ str ++ ")"

sayPoints :: Int -> String
sayPoints n@1 = show n ++ " point"
sayPoints n = show n ++ " points"

-- did you get points because you had the most of something?  the least?  everyone had the same?
data ComparativeScoreReason = Min | Max | AllEqual | Neutral deriving (Show, Eq)

scoreEpochForPlayer :: Bool -> [Int] -> [Int] -> Player -> Player
scoreEpochForPlayer isFinal pharCounts sunTotals p = p { score = max 0 (score p + total) }
  where 
     total :: Int
     total = sum $ map scoreFrom $ trace (unlines $ map explainScore componentScores) componentScores
     componentScores = [ godScore
                       , pharaohScore
                       , nileScore
                       , civScore
                       , goldScore
                       ]
                       ++ (if isFinal then [monumentScore, sunScore] else [])

     godScore  = ScGods  (2*num God)
     goldScore = ScGolds (3*num Gold)
     pharaohScore | length (nub pharCounts) < 2       = ScPharaohs 0    (num Pharaoh, AllEqual)
                  | num Pharaoh == minimum pharCounts = ScPharaohs (-2) (num Pharaoh, Min)
                  | num Pharaoh == maximum pharCounts = ScPharaohs 5    (num Pharaoh, Max)
                  | otherwise                         = ScPharaohs 0    (num Pharaoh, Neutral)
     sunScore     | length (nub sunTotals) < 2        = ScSuns     0    (sunTotal, AllEqual)
                  | sunTotal == minimum sunTotals     = ScSuns     (-5) (sunTotal, Min)
                  | sunTotal == maximum sunTotals     = ScSuns     5    (sunTotal, Max)
                  | otherwise                         = ScSuns     0    (sunTotal, Neutral)
     sunTotal = totalSunCount (suns p)
     nileScore = if num Flood == 0 
       then ScNiles 0 (num Nile, num Flood)
       else ScNiles (num Nile + num Flood) (num Nile, num Flood)
     civScore = ScCivs ([-5, 0, 0, 5, 10, 15] !! numCivTypes) numCivTypes
     numCivTypes = length $ nub [t | Civilization t <- tiles p]
     monumentScore = scoreMonuments monumentTypes
     monumentTypes = [t | Monument t <- tiles p]
     -- so useful!
     num :: StoreableTile -> Int
     num t = length $ filter (==t) $ tiles p 

scoreMonuments :: [MonumentType] -> ScoreReason
scoreMonuments ts = ScMonuments scoreForDifferents scoreForIdenticals
  where
    scoreForDifferents = [0,1,2,3,4,5,6,10,15] !! length (nub ts)
    scoreForIdenticals = sum $ map scoreIdenticalGroup $ filter ( (>=3) . length) $ group $ sort ts
    scoreIdenticalGroup g = [0,0,0,5,10,15] !! length g

endEpoch :: Board -> (Bool, Board)
endEpoch b = case epoch b of
  Epoch 3 -> (True, scoreEpoch $ b { block = [] })
  _other  -> (False, advanceEpoch $ forAllPlayers removeTempTiles $ scoreEpoch $ forAllPlayers faceSunsUp $ b { block = [], raCount = 0 })
    where faceSunsUp = modSuns turnSunsFaceUp

endEpochIntoScoring :: Board -> Board
endEpochIntoScoring b = toMode (scoreEpoch $ forAllPlayers faceSunsUp b ) (ShowScoring (epoch b))
    where faceSunsUp = modSuns turnSunsFaceUp

beginNewEpoch :: Board -> Board
beginNewEpoch b = case epoch b of
  Epoch 3 -> b
  Epoch _ -> toMode (advancePlayer $ advanceEpoch $ forAllPlayers removeTempTiles $ b { block = [], raCount = 0 } ) StartTurn

forAllPlayers :: (Player -> Player) -> Board -> Board
forAllPlayers f b = b{ players = M.map f (players b) } 

initDeck :: [Tile] -> IO [Tile]
initDeck = shuffle

blockMax :: Int
blockMax = 8

raTrackFull :: Board -> Bool
raTrackFull b = (>=mx) . raCount $ b
  where mx = raCountMax b

incRaCount :: Board -> Board
incRaCount ( b@Board{ raCount = rc }) = b { raCount = rc + 1 }

deckEmpty ::  Board -> Bool
deckEmpty = null . deck


testDataMonumentScoring ::  (Integer, [MonumentType])
testDataMonumentScoring =  (19, replicate 4 Pyramid ++ replicate 3 Temple ++ replicate 2 Fortress ++ [Sphinx])

data AuctionReason = BlockFull | RaDrawn | RaCalledVoluntarily deriving (Eq, Show)

readInt :: String -> Maybe Int
readInt str = case reads str of
  []        -> Nothing
  (i, ""):_ -> Just i  -- must be read completely
  _         -> Nothing

type PlayerNum = Int

-- This turns out to be more brittle than simply maintaining a flag within the board,
-- as it relies on endEpoch not clearing the raCount, for example, on ending Epoch 3.
isGameOver :: Board -> Bool
isGameOver b = deckEmpty b || 
 (epoch b == Epoch 3 && (noOneLeftInPlay b || raTrackFull b))

someoneIsStillInPlay :: Board -> Bool
someoneIsStillInPlay b = any (isStillInPlay b) $ playerIds b

noOneLeftInPlay :: Board -> Bool
noOneLeftInPlay = not . someoneIsStillInPlay

isStillInPlay :: Board -> PlayerNum -> Bool
isStillInPlay b pi = not . null . faceUpSuns $ handOf pi b

handOf :: PlayerNum -> Board -> Player
handOf pi b = players b M.! pi



relatedToDisaster :: DisasterType -> [StoreableTile]
relatedToDisaster Drought = [Flood, Nile]
relatedToDisaster Funeral = [Pharaoh]
relatedToDisaster Unrest = map Civilization [minBound .. maxBound]
relatedToDisaster Earthquake = map Monument [minBound .. maxBound]
isRelatedToDisaster :: DisasterType -> StoreableTile -> Bool
isRelatedToDisaster dt t = t `elem` relatedToDisaster dt

canBidHigherThan :: (PlayerNum, Board) -> Sun -> Bool
canBidHigherThan (pi, board) bid = 
  case ss of
     [] -> False
     _  -> bestSun > bid
    where
      bestSun = maximum ss
      ss = faceUpSuns $ handOf pi board

playersForAuction :: Board -> [PlayerNum]
playersForAuction b = filter (isStillInPlay b) $ take (numPlayers b) $ drop 1 $ playerCycle b

faceUpSuns :: Player -> [Sun]
faceUpSuns = fst . suns

faceUpSunsBeating :: Maybe Sun -> Player -> [Sun]
faceUpSunsBeating sM p = maybe fuss (\s -> filter (>s) fuss) sM
  where fuss = faceUpSuns p

hasFaceUpSunBeating :: Maybe Sun -> Player -> Bool
hasFaceUpSunBeating s p = not $ null $ faceUpSunsBeating s p

-- Note: must return in same order as given players
whoCanBeat :: Maybe Sun -> [PlayerNum] -> Board -> [PlayerNum]
whoCanBeat sM pis b = filter (hasFaceUpSunBeating sM . (`handOf` b)) pis

modSuns :: (SunsUpDown -> SunsUpDown) -> Player -> Player
modSuns f p = p { suns = f (suns p) } 
modTiles :: ([StoreableTile] -> [StoreableTile]) -> Player -> Player
modTiles f p = p { tiles = f (tiles p) }

addToTilesOf :: PlayerNum -> [StoreableTile] -> Board -> Board
addToTilesOf pi ts b = b { players = M.adjust (modTiles (++ ts)) pi (players b) }

removeFromTilesOf :: PlayerNum -> [StoreableTile] -> Board -> Board
removeFromTilesOf pi ts b = b { players = M.adjust (modTiles (\\ ts)) pi (players b) }

-- wins an auction, resolving given disasters with the given discards
winAuction ::  PlayerNum -> [Tile] -> [DisasterResolution] -> Board -> Sun -> Board
winAuction pi ts disasterResolutions b winningSun = 
  exchangeSun pi winningSun .
  resolveDisasters pi disasterResolutions .
  wipeBlock .
  addToTilesOf pi nonDisasterTiles $ b

    where nonDisasterTiles = [t | Storeable t <- ts]

-- shared between winAuction and exchangeGod
resolveDisasters :: PlayerNum -> [DisasterResolution] -> Board -> Board
resolveDisasters pi rs b = foldl' (resolveDisaster pi) b rs
resolveDisaster :: PlayerNum -> Board -> DisasterResolution -> Board
resolveDisaster pi b (_disasterType, discards) = traceShow ("resolving disaster", pi, discards) $ removeFromTilesOf pi discards b

type SunsUpDown = ([Sun], [Sun])
type DisasterResolution = (DisasterType, [StoreableTile])

turnSunsFaceUp :: SunsUpDown -> SunsUpDown
turnSunsFaceUp (ups, downs) = (ups ++ downs, [])

numberOfSuns :: Board -> Int
numberOfSuns b = length (fuss++fdss)
  where (fuss,fdss) = suns . head . M.elems . players  $ b

exchangeSun:: PlayerNum -> Sun -> Board -> Board
exchangeSun pi toBoard b = 
  b { boardSun = toBoard
    , players = M.adjust (modSuns f) pi (players b) 
    }
  where f (ups, downs) = (ups \\ [toBoard], boardSun b:downs)

exchangeGod :: PlayerNum -> Tile -> [DisasterResolution] -> Board -> Board
exchangeGod pi t disResns b = 
  traceShow ("exchanging god t ", t, " with drs", disResns) $ 
  resolveDisasters pi disResns $ removeFromBlock [t] $ removeFromTilesOf pi [God] $ addToTilesOf pi storeableTile b
  where
   removeFromBlock :: [Tile] -> Board -> Board
   removeFromBlock ts brd = traceShow ("removeFromBlock", ts) $ brd { block = block brd \\ ts }
   storeableTile= [st | Storeable st <- [t]]


currentPlayerCanUseGod ::  Board -> Bool
currentPlayerCanUseGod board = playerHasGodTile && blockHasGoddableTiles
  where
     playerHasGodTile      = elem God . tiles . active $ board
     blockHasGoddableTiles = any isGoddable . block $ board

data PossibleAction 
            = PADrawTile
            | PACallRa AuctionReason 
            | PAEnterGodMode
            | PAPickTileToGod [Tile]
            | PAFinishWithGods
            | PABeginNewEpoch
            | PADiscardTiles DisasterType [StoreableTile]
            | PABidASun [Sun]
            | PAPass
            deriving (Show, Eq)

data ActualAction
            = AADrawTile
            | AACallRa AuctionReason 
            | AAEnterGodMode
            | AAPickTileToGod Tile
            | AAFinishWithGods
            | AABeginNewEpoch
            | AADiscardTile DisasterType StoreableTile
            | AABidASun PlayerNum Sun
            | AAPass
            deriving (Show, Eq)


data GameMode  = StartTurn
               | InAuction AuctionReason [PlayerNum] (Maybe (PlayerNum,Sun))
               | UsingGod Bool
               | ShowScoring Epoch
               | ResolveDisasters ([StoreableTile], [DisasterResolution], [DisasterType]) (Maybe StoreableTile) DRContext
               deriving (Show, Eq)

data DRContext = AuctionDRContext PlayerNum Sun
          | GodDRContext
          deriving (Show, Eq)

data AfterDisasterResolutionContext = AuctionContext PlayerNum Sun | GodExchangeContext

toMode :: Board -> GameMode -> Board
toMode b m = b { gameMode = m }

apply :: ActualAction -> Board -> Board

apply AABeginNewEpoch board | epoch board == Epoch 3 = error "apply AABeginNewEpoch from epoch 3!"
                            | otherwise              = beginNewEpoch board

apply AAEnterGodMode board   = toMode board (UsingGod False)

apply AAFinishWithGods board = toMode b' StartTurn
  where b' = advancePlayer board

apply (AAPickTileToGod t) board = 
  traceShow ("apply AAPickTileToGod", t) $
  let tilesInHand = tiles . active $ board
      finishOneGodUse :: [DisasterResolution] -> Board -> Board
      finishOneGodUse drs b = if currentPlayerCanUseGod b'
                                  then toMode b' (UsingGod True)
                                  else toMode (advancePlayer b') StartTurn
                                  where b' = exchangeGod (currentPlayerId b) t drs b
  in
    case t of
    NonStoreable (Disaster dt) -> 
      case tryToAutoResolve (tilesInHand, [], [dt]) dt of --TODO: simpler sig for tryToAutoResolveOne
        (_, [], _)     -> toMode board (ResolveDisasters (tilesInHand, [], [dt]) Nothing GodDRContext)
        (_, drs, _dts) -> finishOneGodUse drs board
    _                          -> finishOneGodUse [] board
          -- TODO: (minor)  reconcile the difference between multi-god use (where the board really changes as each god is used) and multi DR in auctions, where the board is only changed after all DRs have been stored up in a mode


apply AADrawTile board = 
  if deckEmpty board 
  then 
    error "END: no more tiles"
  else 
    let (tile:rest) = deck board
    in trace ("Tile drawn: " ++ showName tile) $
    case tile of
      NonStoreable Ra ->
         let newBoard = incRaCount $ board { deck = rest }
         in if raTrackFull newBoard
              then 
                --TODO: sort out when player gets advanced, especially when no one left in play
                trace "Ra Track Full - Immediate End Of Epoch" $ 
                endEpochIntoScoring . advancePlayer $ newBoard 
              else 
                toMode newBoard (InAuction RaDrawn (playersForAuction newBoard) Nothing)

      _ -> 
         toMode (advancePlayer newBoard) StartTurn
           where newBoard = board { deck = rest, block = tile : block board } 

apply (AACallRa reason) board = 
   trace  ("Choice: Call Ra." ++ show reason)$ 
   toMode board (InAuction reason (playersForAuction board) Nothing)

apply AAPass b = 
  case gameMode b of
     (InAuction reason (_pi:pis) current) -> if null candidates
       then
         biddingEnded reason current b
       else
         toMode b (InAuction reason candidates current)
      where 
        candidates = whoCanBeat (fmap snd current) pis b
     other -> error $ "Pass action taken in invalid mode!: "++show other

apply m@(AADiscardTile _dt t) b  = traceShow (show m) $
   case gameMode b of
     -- first of two picks
     (ResolveDisasters (sts, drs, dts) Nothing context) -> 
         traceShow ("picked one to discard: ", t ) $ toMode b (ResolveDisasters (sts \\ [t], drs, dts) (Just t) context)
     -- second of two picks
     (ResolveDisasters (sts, drs, dts) (Just prevT) context) -> traceShow "picked 2nd discard" $ 
        case dts of
          -- all done this was last one
          -- TODO: if end of an auction, could be all players are out - end of epoch
          [dt] -> if noOneLeftInPlay finalize 
                    then traceShow "no one left in play" $ endEpochIntoScoring finalize
                    else toMode (advancePlayer finalize) StartTurn
                   where finalize = case context of
                                  (AuctionDRContext pi s) -> winAuction pi (block b) (addDR dt) b s
                                  (GodDRContext)          -> exchangeGod (currentPlayerId b) (NonStoreable (Disaster dt)) (addDR dt) b
          -- more DRs to do
          (dt:dts') -> traceShow ("do more DRS", addDR dt) $ toMode b (ResolveDisasters (sts \\ [t], addDR dt, dts') Nothing context)
          []   -> error "BUG: apply AADiscardTile, mode has no dts remaining"
      where
         addDR forDT = drs ++ [(forDT,[prevT,t])] 
     other -> error $ "BUG: apply AADiscardTile in bad mode: " ++ show other


apply (AABidASun pi sun) b = case gameMode b of
  (InAuction reason (_:pis) _) -> 
    if null candidates -- unbeatable bid. instant win.
       then
         biddingEnded reason (Just (pi, sun)) b
       else
         toMode b (InAuction reason candidates bid)
    where 
      bid = Just (pi, sun)
      candidates = whoCanBeat (Just sun) pis b
  other -> error $ "BidASun action taken in invalid mode!: "++show other

   -- TODO: address: 
   {-- 
    if noOneLeftInPlay b
     then snd . endEpoch . advancePlayer $ b
     else advancePlayer $ b
   --}

{--
disResns <- case bestBid of
                   Just (_sun, winner) -> getDisasterResolutionsIO winner (block b) b
                   Nothing            -> return []
  let (newBoard, winr) = case bestBid of
                            Just (sun, winner) -> (winAuction winner (block b) disResns  b sun, Just winner)
                            Nothing            -> (b , Nothing)
  let winnerIdStr = maybe "All Passed" (("Auction won by player " ++) . show) winr
  putStrLn winnerIdStr
  return $ newBoard { block = if reason == BlockFull then [] else block newBoard }
--}
  

wipeBlock :: Board -> Board
wipeBlock b = b { block = [] }
     
-- maybe everyone passed
biddingEnded :: AuctionReason -> Maybe (PlayerNum, Sun) -> Board -> Board
biddingEnded reason bestBidM b = 
  case bestBidM of 
  Just (pi, sun) -> 
     case unresolveds of

       [] -> if noOneLeftInPlay b'
               then endEpochIntoScoring b'
               else toMode (advancePlayer b') StartTurn
         where 
           b' = winAuction pi (block b) drs b sun

       _ -> toMode b (ResolveDisasters (candidatesRemaining, drs, unresolveds) Nothing (AuctionDRContext pi sun))
       -- Only finishing resolution will finally win the auction, and swap suns.
       
     where
       (candidatesRemaining, drs, unresolveds) = tryToAutoResolveAll candidates (disasterTypesIn (block b))
       playerTiles      = tiles $ handOf pi b
       candidates       = playerTiles ++ nonDisasterTiles
       nonDisasterTiles = [t | Storeable t <- block b]
       -- TODO: possibly move to disaster recovery before choose action
     --       Perhaps we always flow through disaster recovery which has nothing to do sometimes

  Nothing        -> toMode b' StartTurn
    where b' = if reason == BlockFull
                 then wipeBlock b
                 else b


disasterTypesIn ::  [Tile] -> [DisasterType]
disasterTypesIn ts = [dt | NonStoreable (Disaster dt) <- ts]

-- (the pool, successful resolutions, unresolvable disasters)
type DRAcc =  ([StoreableTile], [DisasterResolution], [DisasterType])

tryToAutoResolveAll :: [StoreableTile] -> [DisasterType] -> DRAcc
tryToAutoResolveAll ts dts = foldl' tryToAutoResolve (ts, [], []) dts

tryToAutoResolve :: DRAcc -> DisasterType -> DRAcc
tryToAutoResolve ([], resns, _fails) _    = ([], resns, [])
tryToAutoResolve (pool, resns, fails) dt = 
  case sacrificesM of
    Just sacrifices -> (pool \\ sacrifices, resns++[(dt,sacrifices)], fails)
    Nothing         -> (pool, resns, fails ++ [dt])
  where 
    relevant = filter (`elem` relatedToDisaster dt) pool
    sacrificesM = case dt of
     Funeral -> Just $ take 2 relevant
     Drought -> Just $ take 2 $ allz Flood ++ allz Nile
                  where allz t = filter (==t) relevant
     _other  | length relevant <= 2 -> Just $ take 2 relevant -- no choice
             | otherwise            -> Nothing -- not auto resolvable


legalActions :: Board -> GameMode ->  [PossibleAction]

legalActions b StartTurn      = [PACallRa reason] ++ useGodM ++ drawTileM
  where
     reason    = if blockFull b then BlockFull else RaCalledVoluntarily
     useGodM   = [PAEnterGodMode   | currentPlayerCanUseGod b]
     drawTileM = [PADrawTile | not (blockFull b)]

legalActions _b (ShowScoring (Epoch 3))       = []
legalActions _b (ShowScoring (Epoch _))       = [PABeginNewEpoch]

legalActions b (UsingGod usedAtLeastOneGod) =
  PAPickTileToGod goddableTiles : [PAFinishWithGods | usedAtLeastOneGod]
  where 
     goddableTiles = filter isGoddable $ block b

legalActions _b (ResolveDisasters (remainingTiles, _, dt:_) _ _)
     = [PADiscardTiles dt remainingTiles]

legalActions _b (ResolveDisasters (_, _, []) _ _) 
     = error "BUG: legalActions, ResolveDisasters has empty dts"

legalActions _b (InAuction _ [] _curBidM) = error "InAuction with no players!"

legalActions b (InAuction reason (pi:pis) curBidM) = 
  if not $ null legalBids
   then PABidASun legalBids : [PAPass | mayPass ]
   else error $ "Invalid state: InAuction with player " ++ show pi ++ " though player has no legal bids!"
  where 
     legalBids = faceUpSunsBeating (fmap snd curBidM) p
     p = handOf pi b
     mayPass = not (isLast && isNothing curBidM && reason == RaCalledVoluntarily)
     isLast = null pis


maybeAt :: [a] -> Int -> Maybe a
maybeAt xs i | i >= 0 && i < length xs = Just $ xs !! i
             | otherwise               = Nothing

testScoreMonuments ::  Test
testScoreMonuments = TestList 
  [ "Score Monuments - Rules example" ~: ScMonuments 4 15 ~=? scoreMonuments (snd testDataMonumentScoring)
  , "monuments none" ~: ScMonuments 0 0 ~=? scoreMonuments []
  , "monuments" ~: ScMonuments 15 0 ~=? scoreMonuments [minBound .. maxBound]
  , "monuments" ~: ScMonuments 1 0 ~=? scoreMonuments [Pyramid]
  , "monuments" ~: ScMonuments 1 0 ~=? scoreMonuments [Pyramid, Pyramid]
  , "monuments" ~: ScMonuments 1 5 ~=? scoreMonuments [Pyramid, Pyramid, Pyramid]
  , "monuments" ~: ScMonuments 15 120 ~=? scoreMonuments [mt | Monument mt <- allMonuments]
  ]

testAutoResolution :: Test
testAutoResolution = TestList
  [ tryToAutoResolveAll [Pharaoh] [Funeral, Drought, Earthquake, Unrest] ~?= ([], [(Funeral, [Pharaoh])], [])

  , tryToAutoResolveAll [] [Funeral, Drought, Earthquake, Unrest] ~?= ([], [], [])

  , let hand = [Gold, Civilization Art, Civilization Art, Civilization Agriculture] 
    in  "3 civs" ~: tryToAutoResolveAll hand [Unrest] ~?= (hand, [], [Unrest])

  , let hand = [Civilization Art, Civilization Agriculture] 
    in "just 2 civs" ~: tryToAutoResolveAll hand [Unrest] ~?= ([], [(Unrest, hand)], [])

  , tryToAutoResolveAll [Nile, Flood, Nile] [Drought] ~?= ([Nile], [(Drought, [Flood, Nile])], [])

  , tryToAutoResolveAll [Gold, Pharaoh, Pharaoh, Pharaoh] [Drought, Funeral, Earthquake, Unrest] ~?= 
      ([Gold, Pharaoh], [(Drought, []), (Funeral, [Pharaoh, Pharaoh]), (Earthquake, []), (Unrest, [])], [])
  ]

foo ::  Integer
foo = trace "foo" $ traceShow "foo" 3

tests ::  IO Counts
tests       = runTestTT $ TestList [ testScoreMonuments
                                   , testAutoResolution ]

-- noOneLeftInPlay ::Board -> Bool

