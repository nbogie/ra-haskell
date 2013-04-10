module GUI where

import Data.List(find,(\\),sort)
import Data.Maybe(isJust,isNothing)
import Prelude hiding (pi)
import System.Environment(getArgs)

import Game

main ::  IO ()
main = do
  args <- getArgs
  let usage = "Usage: prog -p <NumPlayers> [-d] (where NumPlayers is in [2..5])"
  let (nPlayers, tileSet) = case args of
           ("-p":nStr:rest) -> if parsedN `elem` [2..5]
                                 then (parsedN, ts)
                                 else error usage
             where 
               ts      = if "-d" `elem` rest then allTilesTweaked else allTiles
               parsedN = read nStr
           _                 -> error usage

  _counts <- tests
  fmap (initBoard nPlayers) (initDeck tileSet) >>= loopIO 

---------------------------------------------------------
-- TOP LEVEL
---------------------------------------------------------

loopIO ::  Board -> IO ()
loopIO board = do
  let 
      pi = currentPlayerId board
  if isGameOver board
  then do
    putStrLn "GAME OVER. Final score follows: "
    putStrLn $ boardToString $ scoreEpoch board
  else
    if not $ isStillInPlay $ handOf pi board
    then do
         playMsg pi "You are being skipped - you have no face-up suns."
         loopIO (advancePlayer board)
    else do
       putStrLn $ boardToString board
       choice <- getChoice pi
       case choice of
         ChoiceDraw -> do
           playMsg pi "Choice: Draw a tile"
           if blockFull board
           then
             playMsg pi "Block is full - you may only call Ra or use a God Tile" >> loopIO board
           else
             drawTileIO board >>= loopIO

         ChoiceQuit -> playMsg pi "Choice: Quit"

         ChoiceUseGod -> do
           playMsg pi "Choice: Use God tile(s)."
           if currentPlayerCanUseGod board
           then useGodMany1TimesIO pi board >>= loopIO . advancePlayer
           else do
             playMsg pi "You have no God tiles to use or there are no tiles to take!"
             loopIO board

         ChoiceShowScores -> do
           putStrLn "Choice: Compute score as though at epoch end."
           putStrLn $ boardToString $ scoreEpoch board
           loopIO board

         ChoiceAdvanceEpoch -> do
           playMsg pi "Choice: End epoch (dev only)"
           loopIO $ (snd . endEpoch) board

         ChoiceCallRa -> do
           playMsg pi "Choice: Call Ra."
           let reason = if blockFull board then BlockFull else RaCalledVoluntarily
           runAuctionIO reason board >>= \b ->
             if noOneLeftInPlay b
             then
               loopIO . snd . endEpoch . advancePlayer $ b
             else
               loopIO . advancePlayer $ b

data PlayChoice = ChoiceDraw | ChoiceCallRa | ChoiceQuit | ChoiceUseGod | ChoiceShowScores | ChoiceAdvanceEpoch deriving (Show, Eq) 

getChoice :: PlayerNum -> IO PlayChoice
getChoice pi = do
   playMsg pi "Enter return (draw tile), g(use god), r(call Ra), or q(quit)."
   l <- getLine
   case l of
      ""    -> return ChoiceDraw
      "e"   -> return ChoiceAdvanceEpoch
      "g"   -> return ChoiceUseGod
      "q"   -> return ChoiceQuit
      "r"   -> return ChoiceCallRa
      "s"   -> return ChoiceShowScores
      other -> putStrLn ("You entered nonsense: " ++ show other) >> getChoice pi


drawTileIO :: Board -> IO Board
drawTileIO board =
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
           return $ (snd . endEpoch . advancePlayer ) newBoard 
         else
           -- note: run the auction, first, then advance the player, then cede control finally
           runAuctionIO RaDrawn newBoard >>= return . advancePlayer
      next -> do
         let newBlock = next : block board
         let newBoard = board { deck = rest, block = newBlock } 
         putStrLn $ boardToString newBoard 
         return $ advancePlayer newBoard



---------------------------------------------------------
-- AUCTIONS
---------------------------------------------------------
runAuctionIO :: AuctionReason -> Board -> IO Board
runAuctionIO reason b = do
  putStrLn $ "An auction!  Reason: " ++ show reason
  putStrLn $ boardToString b
  bestBid  <- findBestBidIO (reason == RaCalledVoluntarily) b (playersForAuction b) Nothing
  disResns <- case bestBid of
                   Just (_sun, winner) -> getDisasterResolutionsIO winner (block b) b
                   Nothing            -> return []
  let (newBoard, winr) = case bestBid of
                            Just (sun, winner) -> (winAuction winner (block b) disResns  b sun, Just winner)
                            Nothing            -> (b , Nothing)
  let winnerIdStr = maybe "All Passed" (("Auction won by player " ++) . show) winr
  putStrLn winnerIdStr
  return $ newBoard { block = if reason == BlockFull then [] else block newBoard }

getBidChoiceIO :: Bool -> Board -> PlayerNum -> Maybe (Sun, PlayerNum) -> IO (Maybe (Sun, PlayerNum))
getBidChoiceIO isMandatory b pi currBid = do
     let possibleBids = map sunValue $ filter ( > maybe (Sun 0) fst currBid) $ faceUpSuns $ handOf pi b
     let passStr = if isMandatory then ".  You must bid as you called Ra: " else " or hit return to pass: "
     playMsg pi $ "Enter bid: " ++ show possibleBids ++ passStr
     l <- getLine
     case l of
       ""    -> if isMandatory 
                then playMsg pi "You must bid" >> getBidChoiceIO isMandatory b pi currBid 
                else return Nothing
       _     -> case readInt l of
         Just i -> if i `elem` possibleBids
                     then return $ Just (Sun i, pi)
                     else playMsg pi "You don't have that sun!" >> getBidChoiceIO isMandatory b pi currBid
         _      -> playMsg pi "What?" >> getBidChoiceIO isMandatory b pi currBid

findBestBidIO :: Bool -> Board -> [PlayerNum] -> Maybe (Sun, PlayerNum)  -> IO (Maybe (Sun, PlayerNum))
findBestBidIO _ _ [] topBid = return topBid
findBestBidIO lastMustBid b (pi:pis) topBid = do
     let isLast = null pis 
     if isStillInPlay (handOf pi b) && maybe True (((pi,b) `canBidHigherThan`) . fst) topBid
     then
        if isLast && isNothing topBid && lastMustBid
        then
           getBidChoiceIO True b pi topBid
        else do
           bc <- getBidChoiceIO False b pi topBid
           let newBid = if isJust bc then bc else topBid
           findBestBidIO lastMustBid b pis newBid
     else do 
       playMsg pi $ "You cannot bid (better than " ++ show topBid ++ ")"
       findBestBidIO lastMustBid b pis topBid

-----------------------------------------------------------------------------
-- DISASTER RESOLUTION
-----------------------------------------------------------------------------
getDisasterResolutionsIO:: PlayerNum -> [Tile] -> Board -> IO [DisasterResolution]
getDisasterResolutionsIO pi gainedTiles b = do
  let disasterTiles    = [t | t@(Disaster _) <- gainedTiles]
      disasterTypes    = [dtyp | Disaster dtyp <- disasterTiles]
      nonDisasterTiles = gainedTiles \\ disasterTiles
      playerTiles      = tiles $ handOf pi b
      candidates       = playerTiles ++ nonDisasterTiles
  getRes candidates disasterTypes
  where
   getRes :: [Tile] -> [DisasterType] -> IO [DisasterResolution]
   getRes _pts [] = return []
   getRes pts dts = do
     chosenDt <- pickOneFromMenuIO show pi dts "Pick a disaster to resolve: "
     discards <- pickDiscardsForDisasterIO pi pts chosenDt
     playMsg pi $ "Discarded "++show discards
     let pts' = pts \\ discards
     let dts' = dts \\ [chosenDt]
     otherResns <- getRes pts' dts'
     return $ (chosenDt, discards) : otherResns

pickDiscardsForDisasterIO ::  PlayerNum -> [Tile] -> DisasterType -> IO [Tile]
pickDiscardsForDisasterIO pi ts dis = pickDis dis
  where 
  relevant= filter (`elem` relatedToDisaster dis) ts
  pickDis Funeral = return $ take 2 relevant
  pickDis Drought = return $ take 2 $ allz Flood ++ allz Nile
            where allz t = filter (==t) relevant
  pickDis _ | length relevant <= 2 = return $ take 2 relevant -- no choice
            | otherwise            = do  -- guaranteed at least two choices
    d1 <- pickOneFromMenuIO show pi (sort relevant)           "Pick first discard"
    d2 <- pickOneFromMenuIO show pi (sort (relevant \\ [d1])) "Pick second discard"
    return [d1,d2]



---------------------------------------------------------
-- GOD USE
---------------------------------------------------------
useGodIO :: PlayerNum -> Board -> IO Board
useGodIO pi b = do
   tile <- pickOneFromMenuIO show pi (filter isGoddable (block b))  
           "Pick a tile from block to take with your god"
   playMsg pi $ "You took (with God tile): " ++ show tile
   disResns <- getDisasterResolutionsIO pi [tile] b
   return $ exchangeGod pi tile disResns b

useGodMany1TimesIO::  PlayerNum -> Board -> IO Board
useGodMany1TimesIO pi board = do
   b <- useGodIO pi board
   if currentPlayerCanUseGod b
   then do
      continueChoice <- pickOneFromMenuIO show pi [UseAnotherGod, FinishTurn] "Use another god?"
      if continueChoice == UseAnotherGod 
      then useGodMany1TimesIO pi b
      else return b
   else return b

data ContinueChoice = UseAnotherGod | FinishTurn deriving (Show, Eq)

---------------------------------------------------------
-- GENERAL UTILS
---------------------------------------------------------
playMsg :: PlayerNum -> String -> IO ()
playMsg pi msg = putStrLn $ "Player " ++ show pi ++ " >> " ++ msg


pickOneFromMenuIO :: (Eq a) => (a -> String) -> PlayerNum -> [a] -> String -> IO a
pickOneFromMenuIO showFn pi items prompt = do
  playMsg pi prompt
  playMsg pi $ unwords $ map (\(n, i) -> show n ++ ":"++ showFn i) mapping
  l <- getLine
  case readInt l >>= itemIfValid of
    Just x -> playMsg pi ("You chose " ++ showFn x) >> return x
    Nothing -> playMsg pi "Invalid choice" >> pickOneFromMenuIO showFn pi items prompt
    where 
      itemIfValid n = fmap snd $ find ((==n) . fst) mapping
      mapping = zip [0..] items

