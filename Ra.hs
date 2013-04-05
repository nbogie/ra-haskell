module Main where
import Shuffle
import Data.List (nub, sort, group)
import Control.Arrow ((&&&))
import Control.Monad (forM)
import Data.Either (lefts, rights)
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

blockFull :: Board -> Bool
blockFull = (>=8) . length . block

blockToSummaryString :: Block -> String
blockToSummaryString  = unlines . map show . sort . map (head &&& length) . group . sort

blockToString :: Block -> String
blockToString = padWith '.' 8 . map toChar
handToString :: Hand -> String
handToString = handTilesToString . tilesInHand
tilesInHand (Hand ts _) = ts
handTilesToString = map toChar . concat . group . sort 

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
                          , "Player:   " ++ show (currentPlayer b) ++ " hand: " ++ handToString (currentHand b)
                          , show $ epoch b
                          -- , blockToSummaryString  $ block b
                          ]
data Board = Board { raCount :: Int
                   , block :: Block
                   , sun :: Sun
                   , epoch :: Epoch
                   , deck :: Deck
                   , hands :: [Hand]
                   , currentPlayer :: Int
                   } deriving (Show, Eq)

newtype Epoch = Epoch { epochInt :: Int } deriving (Eq)
instance Show Epoch where
  show (Epoch i) = "Epoch " ++ show i

numPlayers :: Board -> Int
numPlayers = length . hands

startingSuns = fmap (fmap (fmap Sun)) [ [[9,6,5,2], [8,7,4,3]]
               , [[13,8,5,2], [12, 9, 6, 3], [11, 10, 7, 4]]]
initHands :: Int -> [Hand]
initHands n = map (\suns -> Hand [] $ map Right suns) sunSets
  where sunSets = head $ filter ((==n) . length) startingSuns 
initBoard :: [Tile] -> Board
initBoard ts = Board 
             { raCount = 0
             , block = []
             , sun = Sun 1
             , epoch = Epoch 1
             , deck = ts
             , hands = initHands 3
             , currentPlayer = 0
             } 

data Hand = Hand [Tile] [Either Sun Sun] deriving (Show, Eq)

currentHand ::  Board -> Hand
currentHand board = hands board !! currentPlayer board
tilesInCurrentHand ::  Board -> [Tile]
tilesInCurrentHand = tilesInHand . currentHand
sunsInCurrentHand ::  Board -> [Either Sun Sun]
sunsInCurrentHand = sunsInHand . currentHand
faceUpSunsInCurrentHand ::  Board -> [Sun]
faceUpSunsInCurrentHand = faceUpSuns . currentHand
faceUpSuns :: Hand -> [Sun]
faceUpSuns = rights . sunsInHand
sunsInHand :: Hand -> [Either Sun Sun]
sunsInHand (Hand ts ss) = ss

advancePlayer :: Board -> Board
advancePlayer b = b { currentPlayer = (currentPlayer b + 1) `mod` (numPlayers b) }
advanceEpoch :: Board -> Board
advanceEpoch b = b { epoch = adv (epoch b) } 
  where
   adv (Epoch 1) = Epoch 2
   adv (Epoch 2) = Epoch 3

endEpoch :: Board -> (Bool, Board)
endEpoch b = case epoch b of
  Epoch 3 -> (True, b { block = [] })
  other   -> (False, advanceEpoch $ b { block = [], raCount = 0 })

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
handOf :: PlayerNum -> Board -> Hand
handOf pi b = hands b !! pi

playerCycle b = map (\i -> i `mod` numPlayers b) [currentPlayer b ..]


data BidChoice = Pass | Bid Sun deriving (Show, Eq, Ord)
getBidChoice :: Board -> PlayerNum -> IO BidChoice
getBidChoice b pi = do
     let possibleBids = map (show . sunValue) $ faceUpSuns $ handOf pi b
     putStrLn $ "Enter bid: " ++ show possibleBids ++ " or hit return to pass: "
     l <- getLine
     case l of
       ""    -> return Pass
       other -> case readInt l of
         Just i -> return $ Bid $ Sun i
         _      -> getBidChoice b pi

canBidHigherThan :: (PlayerNum, Board) -> Sun -> Bool
canBidHigherThan (pi, board) bid = 
  case suns of
     [] -> False
     vs -> bestSun > bid
    where
      bestSun :: Sun
      bestSun = maximum suns
      suns :: [Sun]
      suns = faceUpSuns $ handOf pi board

playersForAuction :: Board -> [PlayerNum]
playersForAuction b = take (numPlayers b) $ drop 1 $ playerCycle b
findBestBid :: Board -> [PlayerNum] -> Maybe (Sun, PlayerNum)  -> IO (Maybe (Sun, PlayerNum))
findBestBid b [] topBid = return topBid
findBestBid b (pi:pis) topBid = 
     if isStillInPlay pi b && maybe True (((pi,b) `canBidHigherThan`) . fst) topBid
     then do
       bc <- getBidChoice b pi
       case bc of
         Bid sv -> findBestBid b pis (Just (sv, pi))
         Pass   -> findBestBid b pis topBid
     else do 
       putStrLn "You cannot bit (better than x)"
       findBestBid b pis topBid

addToHandOf = undefined

af :: AuctionReason -> Board -> IO Board
af reason b = do
  putStrLn $ "An auction!  Reason: " ++ (show reason)
  putStrLn $ boardToString b
  bestBid <- findBestBid b (playersForAuction b) Nothing
  case bestBid of
    Just (sun, winner) -> addToHandOf winner (block b) b
  let isTaken = False
  let nextBlock = case (reason, isTaken) of
                   (_, True)      -> [] 
                   (BlockFull, _) -> [] 
                   _ -> block b
  return $ b { block = nextBlock }

useGodOrCancel :: Board -> IO Board
useGodOrCancel b = putStrLn "Pick a tile from block to take with your god" >> return (advancePlayer b)
currentPlayerCanUseGod board = blockIsNotEmpty board && God `elem` tilesInCurrentHand board



blockIsEmpty :: Board -> Bool
blockIsEmpty = null . block
blockIsNotEmpty :: Board -> Bool
blockIsNotEmpty = not . blockIsEmpty
loop ::  Board -> IO ()
loop board = do
  let keyPrompt = "Enter return (draw tile), g(use god), r(call Ra), or q(quit)."
  putStrLn keyPrompt
  l <- getLine
  case l of
    ""  -> putStrLn "return - Drawing a tile" >> drawTile board af nf >>= loop
    "g" -> do
      putStrLn "g - god"
      if currentPlayerCanUseGod board
      then useGodOrCancel board >>= loop
      else (putStrLn "You have no God tiles to use or there are no tiles to take!  Choose again." >> loop board)
    "r" -> putStrLn "r - calling Ra"
    "q" -> do
      putStrLn "q - quitting"
    other -> do 
      putStrLn $ "You entered nonsense: " ++ show other
      loop board
