module Game where

import Control.Arrow ((&&&))
import Data.List (nub, sort, group, (\\), foldl')
import Debug.Trace (trace)
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

data Tile = Ra 
          | Pharaoh 
          | God 
          | Gold 
          | Nile 
          | Flood 
          | Monument MonumentType
          | Civilization CivilizationType
          | Disaster DisasterType 
          deriving (Eq, Show, Ord)

{--
  We ask isTempTile only of a Tile Storeable, and Ra and Disaster tiles are not storeable.  
  However, Disaster tiles may be taken using a god, that is not storeability but takeability.
  We want to be able to store a mix of such tiles in the deck, such that a Ra and a God and a Flood could all be in the same list of tiles - does this preclude type classes?
--}
isPermanentTile ::  Tile -> Bool
isPermanentTile = not . isTempTile
isTempTile :: Tile -> Bool
isTempTile t = case t of
  Ra             -> True -- TODesign: this question should never be asked
  Pharaoh        -> False
  God            -> True
  Gold           -> True
  Nile           -> False
  Flood          -> True
  Monument _     -> False
  Civilization _ -> True
  Disaster _     -> True --TODesign: this question should never be asked

isGoddable :: Tile -> Bool
isGoddable God = False
-- TODesign: this question should never be asked.  we ask isGoddable of an Tile Auctionable
isGoddable Ra  = False
isGoddable _   = True

-- can be kept in player tile set on auction win or after exchange by god
-- TODesign: solve with types
isStoreable :: Tile -> Bool
isStoreable (Disaster _) = False
isStoreable Ra           = False
isStoreable _            = True

instance ToChar Tile where
   toChar t = case t of
     Ra              -> 'R'
     Pharaoh         -> 'P'
     God             -> 'G'
     Gold            -> '$'
     Nile            -> '-'
     Flood           -> '~'
     Monument mt     -> toChar mt
     Civilization ct -> toChar ct
     Disaster dt     -> toChar dt

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

allTiles :: [Tile]
allTiles = replicate 30 Ra      ++ 
           replicate 8 God      ++ 
           replicate 25 Pharaoh ++ 
           replicate 2 funeral  ++ 
           replicate 25 Nile    ++ 
           replicate 12 Flood   ++ 
           replicate 2 drought  ++ 
           allCivilizations     ++ 
           replicate 4 unrest   ++ 
           replicate 5 Gold     ++ 
           allMonuments         ++ 
           replicate 2 earthquake

-- a tweaked set for an event-heavy game during dev
allTilesTweaked :: [Tile]
allTilesTweaked = replicate 20 Ra ++ 
           replicate 30 God     ++ 
           replicate 25 Pharaoh ++ 
           replicate 4 funeral  ++ 
           replicate 25 Nile    ++ 
           replicate 12 Flood   ++ 
           replicate 4 drought  ++ 
           allCivilizations     ++ 
           replicate 8 unrest   ++ 
           replicate 5 Gold     ++ 
           allMonuments         ++ 
           replicate 6 earthquake

allMonumentTypes ::  [MonumentType]
allMonumentTypes     = [minBound .. maxBound]
allCivilizationTypes ::  [CivilizationType]
allCivilizationTypes = [minBound .. maxBound]

allMonuments :: [Tile]
allMonuments     = concatMap (replicate 5 . Monument) allMonumentTypes
allCivilizations :: [Tile]
allCivilizations = concatMap (replicate 5 . Civilization) allCivilizationTypes

funeral :: Tile
funeral = Disaster Funeral
drought ::  Tile
drought = Disaster Drought
unrest ::  Tile
unrest = Disaster Unrest
earthquake ::  Tile
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
blockFull = (>=8) . length . block

blockToSummaryString :: Block -> String
blockToSummaryString  = unlines . map show . sort . map (head &&& length) . group . sort

blockToString :: Block -> String
blockToString = padWith '.' 8 . map toChar
playerToString ::(PlayerNum, Player) -> String
playerToString (i, p) = "Player: " ++ show i ++ ": (" ++ show (score p) ++ "): " ++ show (suns p) ++ ", " ++ ts
  where ts            = tilesToString . tiles $ p
        tilesToString = map toChar . concat . group . sort 

raTrackToString ::  Board -> String
raTrackToString b = padWith '.' (raCountMax (numPlayers b)) $ replicate (raCount b) 'R'

padWith ::  a -> Int -> [a] -> [a]
padWith c n s | length s >= n = s
              | otherwise     = replicate padding c ++ s
   where padding = n - length s


-- given a number of players, report the max number of spaces in ra track
raCountMax :: Int -> Int
raCountMax 2 = 6
raCountMax 3 = 8
raCountMax 4 = 9
raCountMax 5 = 10
raCountMax other = error $ "BUG in raCountMax: illegal number of players: " ++ show other

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
                     , tiles :: [Tile]
                     , score :: Int
                     } deriving (Show, Eq)

data Board = Board { raCount :: Int
                   , block :: Block
                   , boardSun :: Sun
                   , epoch :: Epoch
                   , deck :: Deck
                   , players :: M.Map PlayerNum Player
                   , currentPlayerId :: Int
                   } deriving (Show, Eq)

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
initBoard numPlayers ts = Board 
             { raCount = 0
             , block = []
             , boardSun = Sun 1
             , epoch = Epoch 1
             , deck = ts
             , players = M.fromList $ zip (playerCycleFromTo 0 numPlayers) (initPlayers numPlayers) 
             , currentPlayerId = 0
             } 

playerCycleFromTo :: PlayerNum -> PlayerNum -> [PlayerNum]
playerCycleFromTo pi mx = map (`mod` mx) [pi ..]
playerCycle ::  Board -> [PlayerNum]
playerCycle b = playerCycleFromTo  (currentPlayerId b) (numPlayers b)

active ::  Board -> Player
active board = players board M.! currentPlayerId board

advancePlayer :: Board -> Board
advancePlayer b = b { currentPlayerId = (currentPlayerId b + 1) `mod` numPlayers b }
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
     num :: Tile -> Int
     num t = length $ filter (==t) $ tiles p 

scoreMonuments :: [MonumentType] -> ScoreReason
scoreMonuments ts = ScMonuments scoreForDifferents scoreForIdenticals
  where
    scoreForDifferents = [0,1,2,3,4,5,6,10,15] !! length (nub ts)
    scoreForIdenticals = sum $ map scoreIdenticalGroup $ filter ( (>=3) . length) $ group $ sort ts
    scoreIdenticalGroup g = [0,0,0,5,10,15] !! length g
endEpoch :: Board -> (Bool, Board)
endEpoch b = case epoch b of
  Epoch 3 -> (True, scoreEpoch $ b { raCount = 0, block = [] })
  _other  -> (False, advanceEpoch $ forAllPlayers removeTempTiles $ scoreEpoch $ forAllPlayers faceSunsUp $ b { block = [], raCount = 0 })
    where faceSunsUp p = modSuns turnSunsFaceUp p

forAllPlayers :: (Player -> Player) -> Board -> Board
forAllPlayers f b = b{ players = M.map f (players b) } 

initDeck :: [Tile] -> IO [Tile]
initDeck = shuffle

raTrackFull :: Board -> Bool
raTrackFull = (>=8) . raCount

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

isStillInPlay :: PlayerNum -> Board -> Bool
isStillInPlay pi b = not $ null $ faceUpSuns $ handOf pi b
handOf :: PlayerNum -> Board -> Player
handOf pi b = players b M.! pi



relatedToDisaster :: DisasterType -> [Tile]
relatedToDisaster Drought = [Flood, Nile]
relatedToDisaster Funeral = [Pharaoh]
relatedToDisaster Unrest = map Civilization [minBound .. maxBound]
relatedToDisaster Earthquake = map Monument [minBound .. maxBound]


canBidHigherThan :: (PlayerNum, Board) -> Sun -> Bool
canBidHigherThan (pi, board) bid = 
  case ss of
     [] -> False
     _  -> bestSun > bid
    where
      bestSun = maximum ss
      ss = faceUpSuns $ handOf pi board

playersForAuction :: Board -> [PlayerNum]
playersForAuction b = take (numPlayers b) $ drop 1 $ playerCycle b

faceUpSuns :: Player -> [Sun]
faceUpSuns = fst . suns

modSuns :: (SunsUpDown -> SunsUpDown) -> Player -> Player
modSuns f p = p { suns = f (suns p) } 
modTiles :: ([Tile] -> [Tile]) -> Player -> Player
modTiles f p = p { tiles = f (tiles p) }

addToTilesOf :: PlayerNum -> [Tile] -> Board -> Board
addToTilesOf pi ts b = b { players = M.adjust (modTiles (++ storeables)) pi (players b) }
  where storeables = filter isStoreable ts

removeFromTilesOf :: PlayerNum -> [Tile] -> Board -> Board
removeFromTilesOf pi ts b = b { players = M.adjust (modTiles (\\ ts)) pi (players b) }

-- wins an auction, resolving given disasters with the given discards
winAuction ::  PlayerNum -> [Tile] -> [DisasterResolution] -> Board -> Sun -> Board
winAuction pi ts disasterResolutions b winningSun = 
  exchangeSun pi winningSun .
  resolveDisasters pi disasterResolutions .
  wipeBlock .
  addToTilesOf pi nonDisasterTiles $ b

    where  wipeBlock brd = brd { block = []} 
           nonDisasterTiles = ts \\ disasterTiles
           disasterTiles    = [t | t@(Disaster _) <- ts]

-- shared between winAuction and exchangeGod
resolveDisasters :: PlayerNum -> [DisasterResolution] -> Board -> Board
resolveDisasters pi rs b = foldl' (resolveDisaster pi) b rs
resolveDisaster :: PlayerNum -> Board -> DisasterResolution -> Board
resolveDisaster pi b (_disasterType, discards) = removeFromTilesOf pi discards b

type SunsUpDown = ([Sun], [Sun])
type DisasterResolution = (DisasterType, [Tile])

turnSunsFaceUp :: SunsUpDown -> SunsUpDown
turnSunsFaceUp (ups, downs) = (ups ++ downs, [])

exchangeSun:: PlayerNum -> Sun -> Board -> Board
exchangeSun pi toBoard b = 
  b { boardSun = toBoard
    , players = M.adjust (modSuns f) pi (players b) 
    }
  where f (ups, downs) = (ups \\ [toBoard], boardSun b:downs)

exchangeGod :: PlayerNum -> Tile -> [DisasterResolution] -> Board -> Board
exchangeGod pi t disResns b = 
  resolveDisasters pi disResns $ removeFromBlock [t] $ removeFromTilesOf pi [God] $ addToTilesOf pi gainableTile b
  where
   removeFromBlock :: [Tile] -> Board -> Board
   removeFromBlock ts brd = brd { block = block brd \\ ts }
   gainableTile = [t | isStoreable t]


currentPlayerCanUseGod ::  Board -> Bool
currentPlayerCanUseGod board = playerHasGodTile && blockHasGoddableTiles
  where
     playerHasGodTile      = elem God . tiles . active $ board
     blockHasGoddableTiles = any isGoddable . block $ board




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

tests ::  IO Counts
tests       = runTestTT $ TestList [ testScoreMonuments]
