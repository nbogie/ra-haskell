module Main where
import Shuffle
import Data.List (nub, sort, group, (\\), find)
import Data.Maybe (fromMaybe, isJust)

import Control.Arrow ((&&&))
import Control.Monad (forM)
import qualified Data.Map as M
class ToChar a where
  toChar :: a -> Char

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

isTempTile :: Tile -> Bool
isTempTile t = case t of
  Ra             -> True -- TODO: this question should never be asked
  Pharaoh        -> False
  God            -> True
  Gold           -> True
  Nile           -> False
  Flood          -> True
  Monument _     -> False
  Civilization _ -> True
  Disaster _     -> True --TODO: this question should never be asked

isGoddable :: Tile -> Bool
isGoddable God = False
isGoddable Ra  = False -- TODO: this question should never be asked.  we ask isGoddable of an Tile Auctionable
isGoddable _   = True

instance ToChar Tile where
   toChar t = case t of
     Ra              -> 'R'
     Pharaoh         -> 'P'
     God             -> 'G'
     Gold            -> '#'
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
     Funeral    -> 'p'
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


allMonumentTypes     = [minBound .. maxBound]
allCivilizationTypes = [minBound .. maxBound]

allMonuments     = concatMap (replicate 5 . Monument) allMonumentTypes
allCivilizations = concatMap (replicate 5 . Civilization) allCivilizationTypes

funeral = Disaster Funeral
drought = Disaster Drought
unrest = Disaster Unrest
earthquake = Disaster Earthquake

toDebugStr :: Tile -> String
toDebugStr t = toChar t : ' ' : show t

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
playerToString :: Player -> String
playerToString p = show (suns p) ++ ", " ++ ts
  where ts            = tilesToString . tiles $ p
        tilesToString = map toChar . concat . group . sort 

raTrackToString ::  Board -> [Char]
raTrackToString b = padWith '.' (raCountMax (numPlayers b)) $ replicate (raCount b) 'R'

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
boardToString b = unlines [ 
                            "Block:    " ++ blockToString (block b)
                          , "Ra Track: " ++ raTrackToString b
                          , "Player:   " ++ show (currentPlayerId b) ++ " hand: " ++ playerToString (active b)
                          , show $ epoch b
                          -- , blockToSummaryString  $ block b
                          ]
data Player = Player { suns :: ([Sun], [Sun])
                     , tiles :: [Tile]
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

startingSuns = fmap (fmap (fmap Sun)) [ [[9,6,5,2], [8,7,4,3]]
               , [[13,8,5,2], [12, 9, 6, 3], [11, 10, 7, 4]]]
initPlayers :: Int -> [Player]
initPlayers n = map (\ss -> Player (ss, []) []) sunSets
  where sunSets = head $ filter ((==n) . length) startingSuns 

initBoard :: [Tile] -> Board
initBoard ts = Board 
             { raCount = 0
             , block = []
             , boardSun = Sun 1
             , epoch = Epoch 1
             , deck = ts
             , players = M.fromList $ zip (playerCycleFromTo 0 3) (initPlayers 3) 
             , currentPlayerId = 0
             } 

playerCycleFromTo pi mx = map (\i -> i `mod` mx) [pi ..]
playerCycle b = playerCycleFromTo  (currentPlayerId b) (numPlayers b)

active ::  Board -> Player
active board = players board M.! currentPlayerId board

advancePlayer :: Board -> Board
advancePlayer b = b { currentPlayerId = (currentPlayerId b + 1) `mod` (numPlayers b) }
advanceEpoch :: Board -> Board
advanceEpoch b = b { epoch = adv (epoch b) } 
  where
   adv (Epoch 1) = Epoch 2
   adv (Epoch 2) = Epoch 3

removeTempTiles :: Player -> Player
removeTempTiles (Player ss ts) = Player ss (filter isTempTile ts)

endEpoch :: Board -> (Bool, Board)
endEpoch b = case epoch b of
  Epoch 3 -> (True, b { block = [] })
  other   -> (False, advanceEpoch $ forAllPlayers removeTempTiles $ forAllPlayers faceSunsUp $ b { block = [], raCount = 0 })
    where faceSunsUp (Player ss ts) = Player (turnSunsFaceUp ss) ts

forAllPlayers :: (Player -> Player) -> Board -> Board
forAllPlayers f b = b{ players = M.map f (players b) } 

initDeck ::  IO [Tile]
initDeck = shuffle allTiles

raTrackFull :: Board -> Bool
raTrackFull = (>=8) . raCount

main ::  IO ()
main = do
  -- mapM_ (putStrLn . toDebugStr) $ sort $ nub allTiles
  fmap initBoard initDeck >>= loop 
incRaCount :: Board -> Board
incRaCount ( b@Board{ raCount = rc }) = b { raCount = rc + 1 }

deckEmpty ::  Board -> Bool
deckEmpty = null . deck

drawTile :: Board -> (AuctionReason -> Board -> IO Board) -> (Board -> IO Board) -> IO Board
drawTile board auctionFn normalFn = do
  if deckEmpty board 
  then do
    print "END - no more tiles"
    return board
  else do
    let (tile:rest) = deck board
    putStrLn $ "Tile drawn: " ++ show tile
    case tile of
      Ra -> do
         let newBoard = incRaCount $ board { deck = rest }
         if raTrackFull newBoard
         then do
           print "Ra Track Full - Immediate End Of Epoch"
           case (endEpoch . advancePlayer ) newBoard of
             (True, b)  -> print "LAST EPOCH.  GAME OVER AFTER FINAL SCORING" >> return b
             (False, b) -> return b
         else
           -- note: run the auction, first, then advance the player, then cede control finally
           fmap advancePlayer $ auctionFn RaDrawn newBoard
      next -> do
         let newBlock = next : block board
         let newBoard = board { deck = rest, block = newBlock } 
         if blockFull newBoard
         then
           fmap advancePlayer $ auctionFn BlockFull newBoard
         else
           fmap advancePlayer $ normalFn newBoard

data AuctionReason = BlockFull | RaDrawn | RaCalled deriving (Eq, Show)

nf :: Board -> IO Board
nf b = do
  putStrLn "A normal tile draw"
  putStrLn $  boardToString b
  return b


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

af :: AuctionReason -> Board -> IO Board
af reason b = do
  putStrLn $ "An auction!  Reason: " ++ (show reason)
  putStrLn $ boardToString b
  bestBid <- findBestBid b (playersForAuction b) Nothing
  let newBoard = case bestBid of
                   Just (sun, winner) -> winAuction winner (block b) b sun
                   Nothing            -> b 
  return $ newBoard { block = if reason == BlockFull then [] else (block newBoard) }

getBidChoice :: Board -> PlayerNum -> Maybe (Sun, PlayerNum) -> IO (Maybe (Sun, PlayerNum))
getBidChoice b pi currBid = do
     let possibleBids = map sunValue $ filter ( > (maybe (Sun 0) fst currBid)) $ faceUpSuns $ handOf pi b
     putStrLn $ show pi ++ ": Enter bid: " ++ show possibleBids ++ " or hit return to pass: "
     l <- getLine
     case l of
       ""    -> return Nothing
       other -> case readInt l of
         Just i -> if i `elem` possibleBids
                     then return $ Just (Sun i, pi)
                     else putStrLn "You don't have that sun!" >> getBidChoice b pi currBid
         _      -> putStrLn "What?" >> getBidChoice b pi currBid

findBestBid :: Board -> [PlayerNum] -> Maybe (Sun, PlayerNum)  -> IO (Maybe (Sun, PlayerNum))
findBestBid b [] topBid = return topBid
findBestBid b (pi:pis) topBid = 
     if isStillInPlay pi b && maybe True (((pi,b) `canBidHigherThan`) . fst) topBid
     then do
       bc <- getBidChoice b pi topBid
       let newBid = if isJust bc then bc else topBid
       findBestBid b pis newBid
     else do 
       putStrLn $ "You cannot bid (better than " ++ show topBid ++ ")"
       findBestBid b pis topBid

canBidHigherThan :: (PlayerNum, Board) -> Sun -> Bool
canBidHigherThan (pi, board) bid = 
  case ss of
     [] -> False
     vs -> bestSun > bid
    where
      bestSun = maximum ss
      ss = faceUpSuns $ handOf pi board

playersForAuction :: Board -> [PlayerNum]
playersForAuction b = take (numPlayers b) $ drop 1 $ playerCycle b

faceUpSuns :: Player -> [Sun]
faceUpSuns = fst . suns

modSuns :: (SunsUpDown -> SunsUpDown) -> Player -> Player
modSuns f (Player ss ts) = Player (f ss) ts
modTiles :: ([Tile] -> [Tile]) -> Player -> Player
modTiles f (Player ss ts) = Player ss (f ts) 

addToTilesOf :: PlayerNum -> [Tile] -> Board -> Board
addToTilesOf pi ts b = b { players = M.adjust (modTiles (++ ts)) pi (players b) }
removeFromTilesOf :: PlayerNum -> [Tile] -> Board -> Board
removeFromTilesOf pi ts b = b { players = M.adjust (modTiles (\\ ts)) pi (players b) }

winAuction ::  PlayerNum -> [Tile] -> Board -> Sun -> Board
winAuction pi ts b lostSun = 
  exchangeSun pi lostSun $ wipeBlock $ addToTilesOf pi ts b
    where  wipeBlock b = b { block = []} 

type SunsUpDown = ([Sun], [Sun])

turnSunsFaceUpFor :: PlayerNum -> Sun -> Board -> Board
turnSunsFaceUpFor pi s b = b { players = M.adjust (modSuns turnSunsFaceUp) pi (players b) }

turnSunsFaceUp :: SunsUpDown -> SunsUpDown
turnSunsFaceUp (ups, downs) = (ups ++ downs, [])

exchangeSun:: PlayerNum -> Sun -> Board -> Board
exchangeSun pi toBoard b = 
  b { boardSun = toBoard
    , players = M.adjust (modSuns f) pi (players b) 
    }
  where f (ups, downs) = (ups \\ [toBoard], (boardSun b):downs)

-- todo: resolve disasters if picked
exchangeGod :: PlayerNum -> Tile -> Board -> Board
exchangeGod pi t b = removeFromTilesOf pi [God] $ addToTilesOf pi [t] b


-- todo: this should be local to useGodOrCancel
tilesOnBlockMapping :: Block -> [(Int, Tile)]
tilesOnBlockMapping bl = zip  [0..] (filter isGoddable bl)


useGodOrCancel :: PlayerNum -> Board -> IO Board
useGodOrCancel pi b = do
  putStrLn "Pick a tile from block to take with your god, or c to cancel"
  putStrLn $ "Tiles on Block: " ++ show (tilesOnBlockMapping (block b))
  l <- getLine
  if l == "c"  -- cancel
  then 
    return b
  else
     case readInt l >>= validOnBlock of
       -- TODO: allow multiple useGodOrCancels, if player has multiple gods
       Just i -> putStrLn ("You chose "++show i) >> (return (advancePlayer (exchangeGod pi Ra b)))
       Nothing -> useGodOrCancel pi b 
       where validOnBlock :: Int -> Maybe Tile
             validOnBlock n =  fmap snd $ find ((==n) . fst) mapping
             numsForBlockTiles =  map fst mapping
             mapping = tilesOnBlockMapping $ block b

currentPlayerCanUseGod board = blockIsNotEmpty board && God `elem` (tiles . active) board

loop ::  Board -> IO ()
loop board = do
  let keyPrompt = "Enter return (draw tile), g(use god), r(call Ra), or q(quit)."
  let pi = currentPlayerId board
  if isStillInPlay pi board then do
     putStrLn $ (show pi) ++ ": " ++ keyPrompt
     putStrLn $ boardToString board
     l <- getLine
     case l of
       ""  -> putStrLn "return - Drawing a tile" >> drawTile board af nf >>= loop
       "g" -> do
         putStrLn "g - god"
         if currentPlayerCanUseGod board
         then useGodOrCancel pi board >>= loop
         else (putStrLn "You have no God tiles to use or there are no tiles to take!  Choose again." >> loop board)
       "r" -> do
         putStrLn "r - calling Ra"
         fmap advancePlayer (af RaCalled board) >>= loop
       "q" -> do
         putStrLn "q - quitting"
       other -> do 
         putStrLn $ "You entered nonsense: " ++ show other
         loop board
     else do
       putStrLn "Skipping player - no suns left"
       loop (advancePlayer board)
