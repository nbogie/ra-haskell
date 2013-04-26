module Game where

import Control.Arrow ((&&&))
import Data.List (nub, sort, group, (\\), foldl')
import Data.Maybe (isNothing)
import Debug.Trace (trace) --, traceShow)
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
   replicate 30 Ra      ++ 
   replicate 4 funeral  ++ 
   replicate 4 drought  ++ 
   replicate 6 unrest   ++ 
   replicate 4 earthquake

allStoreableTilesTweaked :: [StoreableTile]
allStoreableTilesTweaked = 
           replicate 20 God      ++ 
           replicate 25 Pharaoh ++ 
           replicate 25 Nile    ++ 
           replicate 12 Flood   ++ 
           allCivilizations     ++ 
           replicate 5 Gold     ++ 
           allMonuments

allMonumentTypes ::  [MonumentType]
allMonumentTypes     = [minBound .. maxBound]
allCivilizationTypes ::  [CivilizationType]
allCivilizationTypes = [minBound .. maxBound]

allMonuments :: [StoreableTile]
allMonuments     = concatMap (replicate 5 . Monument) allMonumentTypes
allCivilizations :: [StoreableTile]
allCivilizations = concatMap (replicate 5 . Civilization) allCivilizationTypes

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
blockFull = (>=8) . length . block

blockToSummaryString :: Block -> String
blockToSummaryString  = unlines . map show . sort . map (head &&& length) . group . sort

blockToString :: Block -> String
blockToString = padWith '.' 8 . map toChar
playerToString ::(PlayerNum, Player) -> String
playerToString (i, p) = "Player: " ++ show i ++ ": (" ++ show (score p) ++ "): " ++ show (sunsSorted p) ++ ", " ++ ts
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
             , gameMode = ChooseAction
             , block = []
             , boardSun = Sun 1
             , epoch = Epoch 1
             , deck = ts
             , players = M.fromList $ zip (playerCycleFromTo 0 nPlayers) (initPlayers nPlayers) 
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
    where faceSunsUp p = modSuns turnSunsFaceUp p

forAllPlayers :: (Player -> Player) -> Board -> Board
forAllPlayers f b = b{ players = M.map f (players b) } 

initDeck :: [Tile] -> IO [Tile]
initDeck = shuffle

raTrackFull :: Board -> Bool
raTrackFull b = (>=mx) . raCount $ b
  where mx = raCountMax $ numPlayers b

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
someoneIsStillInPlay = or . M.elems . M.map isStillInPlay . players

noOneLeftInPlay :: Board -> Bool
noOneLeftInPlay = not . someoneIsStillInPlay

isStillInPlay :: Player -> Bool
isStillInPlay = not . null . faceUpSuns

handOf :: PlayerNum -> Board -> Player
handOf pi b = players b M.! pi



relatedToDisaster :: DisasterType -> [StoreableTile]
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

faceUpSunsBeating :: Maybe Sun -> Player -> [Sun]
faceUpSunsBeating sM p = maybe fuss (\s -> filter (>s) fuss) sM
  where fuss = faceUpSuns p

hasFaceUpSunBeating :: Maybe Sun -> Player -> Bool
hasFaceUpSunBeating s p = not $ null $ faceUpSunsBeating s p

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
resolveDisaster pi b (_disasterType, discards) = removeFromTilesOf pi discards b

type SunsUpDown = ([Sun], [Sun])
type DisasterResolution = (DisasterType, [StoreableTile])

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
  resolveDisasters pi disResns $ removeFromBlock [t] $ removeFromTilesOf pi [God] $ addToTilesOf pi storeableTile b
  where
   removeFromBlock :: [Tile] -> Board -> Board
   removeFromBlock ts brd = brd { block = block brd \\ ts }
   storeableTile= [st | Storeable st <- [t]]


currentPlayerCanUseGod ::  Board -> Bool
currentPlayerCanUseGod board = playerHasGodTile && blockHasGoddableTiles
  where
     playerHasGodTile      = elem God . tiles . active $ board
     blockHasGoddableTiles = any isGoddable . block $ board

data Action = DrawTile
            | CallRa AuctionReason 
            | UseGod
            | PickTileToGod
            | UseAnotherGod
            | FinishWithGods
            | ChooseDisasterToResolve
            | ChooseTilesToDiscard
            | BidASun PlayerNum Sun
            | Pass deriving (Show, Eq)

data GameMode  = ChooseAction
               | InAuction AuctionReason [PlayerNum] (Maybe (PlayerNum,Sun))
               | UsingGod
               | AfterUseGod
               | ResolveDisasters1
               | ResolveDisasters2 deriving (Show, Eq)


toMode :: Board -> GameMode -> Board
toMode b m = b { gameMode = m }
apply :: Action -> Board -> Board
apply DrawTile board = 
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
                trace "Ra Track Full - Immediate End Of Epoch"
                toMode (snd . endEpoch . advancePlayer $ newBoard) ChooseAction
              else 
                toMode newBoard (InAuction RaDrawn (playersForAuction newBoard) Nothing)

      _ -> 
         toMode (advancePlayer newBoard) ChooseAction
           where newBoard = board { deck = rest, block = tile : block board } 

apply (CallRa reason) board = 
   trace  ("Choice: Call Ra." ++ show reason)$ 
   toMode board (InAuction reason (playersForAuction board) Nothing)

apply Pass b = case gameMode b of
  (InAuction reason pis current) -> if null candidates
    then
      finishAuction reason current b
    else
      toMode b (InAuction reason candidates current)
   where 
     candidates = whoCanBeat (fmap snd current) pis b
  other -> error $ "Pass action taken in invalid mode!: "++show other

apply (BidASun pi sun) b = case gameMode b of
  (InAuction reason (_:pis) _) -> 
    if null candidates -- unbeatable bid. instant win.
       then
         finishAuction reason (Just (pi, sun)) b
       else
         toMode b (InAuction reason candidates bid)
    where 
      bid = (Just (pi, sun))
      candidates = whoCanBeat (Just sun) pis b
  other -> error $ "BidASun action taken in invalid mode!: "++show other

-- hasFaceUpSunBeating :: Maybe Sun -> Player -> Bool
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
  
apply _ board = board

wipeBlock :: Board -> Board
wipeBlock b = b { block = [] }
     
finishAuction :: AuctionReason -> Maybe (PlayerNum, Sun) -> Board -> Board
finishAuction reason bidM b = 
  case bidM of 
  Just (pi, sun) -> 
     -- TODO: possibly move to disaster recovery before choose action
     --       Perhaps we always flow through disaster recovery which has nothing to do sometimes
     let disasterResns = []
         b' = advancePlayer $ winAuction pi (block b) disasterResns b sun
     in toMode b' ChooseAction -- TODO: advance player
  Nothing        -> toMode b' ChooseAction
    where b' = if reason == BlockFull
                 then wipeBlock b
                 else b

-- Note: must return in order
whoCanBeat :: Maybe Sun -> [PlayerNum] -> Board -> [PlayerNum]
whoCanBeat sM pis b = filter (hasFaceUpSunBeating sM . (`handOf` b)) pis



legalActions :: Board -> GameMode ->  [Action]
legalActions b ChooseAction      = [CallRa reason] ++ useGodM ++ drawTileM
  where
     reason    = if blockFull b then BlockFull else RaCalledVoluntarily
     useGodM   = [UseGod   | currentPlayerCanUseGod b]
     drawTileM = [DrawTile | not (blockFull b)]
legalActions _b UsingGod          = [PickTileToGod]
legalActions  b AfterUseGod       = if currentPlayerCanUseGod b then [UseAnotherGod, FinishWithGods] else []
legalActions _b ResolveDisasters1 = [ChooseDisasterToResolve]
legalActions _b ResolveDisasters2 = [ChooseTilesToDiscard] -- TODO: no action if auto-resolveable
legalActions _b (InAuction _ [] _curBidM) = error "InAuction with no players!"
legalActions b (InAuction reason (pi:pis) curBidM) = 
  if (not $ null $ legalBids) 
   then map (BidASun pi) legalBids ++ [Pass | mayPass ]
   else []
  where 
     legalBids = faceUpSunsBeating (fmap snd curBidM) p
     p = handOf pi b
     mayPass = not (isLast && isNothing curBidM && reason == RaCalledVoluntarily)
     isLast = null pis



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
