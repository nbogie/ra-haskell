module Main where
import Control.Monad (mfilter)
import Data.Array
import Data.List(find, sort)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe(fromMaybe,mapMaybe, isJust, maybeToList)
-- import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
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

  _counts  <- tests
  contents <- readFile "sprites.dat"
  let sprs =  readCharsMap (lines contents)
  board <- fmap (initBoard nPlayers) (initDeck tileSet)
  guimain board sprs

colorFor :: Int -> Color
colorFor i = colrs !! (i `mod` length colrs)
  where colrs = [black, white, green, yellow, red, blue, orange
                , chartreuse, azure, black, aquamarine, rose
                , cyan, magenta]
type MySprite = (String, GridArray)
type GridArray = Array (Int, Int) Int

guimain :: Board -> M.Map String MySprite -> IO ()
guimain board sprs = 
  playIO
          (InWindow "Ra - Sprite GUI" --name of the window
            (900,800) -- initial size of the window
            (0, 0) -- initial position of the window
          )
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (initGS board sprs) -- the initial world
          (return . drawState) -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (\i w -> return $ updateGame i w)

updateGame :: Float  -> GS -> GS
updateGame _step gs = gs { frame = frame gs + 1 }

initGS ::  Board -> SpriteMap -> GS
initGS b sprs = GS 
   { raBoard = b
   , frame = 0
   , cursors = M.fromList [(BlockCursor, (0,0)), (StoreCursor, (0,0)), (SunCursor, (0,0))]
   , cursorPos = (0,0)
   , curSprite = initSprite
   , sprites = sprs}

initSprite ::  (String, Array (Int, Int) Int)
initSprite = ("untitled", array ((0,0), (7,7)) [((x,y),0) | x<- [0..7], y <- [0..7]])

backgroundColor ::  Color
backgroundColor = colorSea 


data InputHandler 
  = InKey Char 
  | CursorSelection CursorGroup 
  deriving (Show, Eq)

data CursorGroup 
  = CSGoddableTiles [Tile] 
  | CSDiscardables DisasterType [StoreableTile] 
  | CSSuns [Sun] 
  deriving (Show, Eq)

inpFor :: PossibleAction -> InputHandler
inpFor PADrawTile             = InKey 'd'
inpFor (PACallRa _)           = InKey 'r'
inpFor PAEnterGodMode         = InKey 'g'
inpFor (PAPickTileToGod ts)   = CursorSelection (CSGoddableTiles ts)
inpFor PAFinishWithGods       = InKey 'p'
inpFor (PADiscardTiles dt ts) = CursorSelection $ CSDiscardables dt ts
inpFor (PABidASun ss)         = CursorSelection $ CSSuns ss -- InKey $ chr $ ord 'a' + (sunValue sun) - 1
inpFor PAPass                 = InKey 'p'
inpFor PABeginNewEpoch        = InKey 'c'

handleInput :: Event -> GS -> IO GS
handleInput e gs = return $ handleWithAll handlers e gs
  where
    handlers = handlersForLegalAcs gs

type FullInputHandler = (InputHandler, PossibleAction)
handleWithAll :: [(InputHandler, PossibleAction)] -> Event -> GS -> GS
handleWithAll hs e gs = fromMaybe gs (firstHandlerM >>= handle e gs)
  where 
    firstHandlerM :: Maybe FullInputHandler 
    firstHandlerM = find (isJust . handle e gs) hs

handle :: Event -> GS -> FullInputHandler -> Maybe GS
handle (EventKey (Char cE) Down _mods _) gs (InKey cI, paction) = if cE == cI 
                                                                   then Just $ applyAction (concretize paction) gs
                                                                   else Nothing
   where 
      concretize PADrawTile         = AADrawTile
      concretize (PACallRa r)       = AACallRa r
      concretize (PAPass)           = AAPass
      concretize (PAEnterGodMode)   = AAEnterGodMode
      concretize (PAFinishWithGods) = AAFinishWithGods
      concretize (PABeginNewEpoch)  = AABeginNewEpoch
      concretize other              = error $ "BUG: asked to handle a key for unknown possible-action: " ++ show other

handle (EventKey (SpecialKey KeyEnter) Down _ _) gs (CursorSelection cursorGroup, _paction) = 
  case actionM of
    Nothing  -> Nothing
    Just action -> Just $ applyAction action gs
    where 
      actionM = case cursorGroup of
         --picking tile(s) to god
         (CSGoddableTiles ts) -> 
           case mfilter (`elem` ts) tileAtCursorM of
              Just tile -> Just $ AAPickTileToGod tile
              _         -> Nothing
           where
             tileAtCursorM = maybeAt blockTiles x
             blockTiles = block $ raBoard gs
             (x, _) = blockCursor gs


         -- resolving disasters
         (CSDiscardables dt discardables) -> 
           case mfilter isLegalChoice tileAtCursorM of
            Just tile -> Just $ AADiscardTile dt tile
            _         -> Nothing
           where
             isLegalChoice t = t `elem` discardables && isRelatedToDisaster dt t
             (ts, posns, _countMs) = storeOrWonTilesAtPositions gs
             tileAtCursorM :: Maybe StoreableTile
             tileAtCursorM = case find ((==pos). fst) $ zip posns ts of
                               Just (_p, (t,True)) -> Just t
                               _ -> Nothing
               where 
                 pos = storeCursor gs

         --bidding
         (CSSuns ss) -> if sunAtCursor `elem` ss
                           then Just (AABidASun pi sunAtCursor)
                           else Nothing -- not a legal bid
      (InAuction _reason (pi:_) _curBid) = gameMode $ raBoard gs
      sunAtCursor = sunAtIndex x
        where
          (x,_y) = sunCursor gs
          (fuSuns, fdSuns) = suns $ currentOrAuctionCurrentPlayer $ raBoard gs
          sunAtIndex i = (fuSuns ++ fdSuns) !! i

handle (EventKey key Down _ _) gs (CursorSelection cursorGroup, _paction) = 
  handleCursor key cursorGroup  gs
handle _e _gs _h = Nothing -- traceShow ("no matching Input Handler", e, h) $ Nothing

handlersForLegalAcs ::  GS -> [FullInputHandler]
handlersForLegalAcs gs = zip (map inpFor legalAcs) legalAcs
 where
   mode = gameMode $ raBoard gs
   legalAcs = legalActions (raBoard gs) mode


applyAction ::  ActualAction -> GS -> GS
applyAction ac  gs = gs { raBoard = newBoard } 
  where newBoard = apply ac $ raBoard gs 

changeCursor :: CursorGroup -> CursorDir -> GS -> GS
changeCursor cg d gs = modCursor (capPos (limits cursorType) . changePos d)
  where 
    modCursor f = gs { cursors = M.adjust f cursorType (cursors gs) }
    cursorType = case cg of
       (CSGoddableTiles _) -> BlockCursor
       (CSDiscardables _ _) -> StoreCursor
       (CSSuns _)         -> SunCursor
    limits SunCursor = ((0,0), (numberOfSuns (raBoard gs) - 1,0))
    limits BlockCursor = ((0,0), (blockMax - 1,0))
    limits StoreCursor = ((0,0), (8,1))
data CursorType = BlockCursor | StoreCursor | SunCursor deriving (Show, Eq, Ord)

handleCursor ::  Key -> CursorGroup -> GS -> Maybe GS
handleCursor k cg gs  = 
  case k of
  (SpecialKey KeyDown)  -> Just $ changeCursor cg CDown gs
  (SpecialKey KeyUp)    -> Just $ changeCursor cg CUp gs
  (SpecialKey KeyLeft)  -> Just $ changeCursor cg CLeft gs
  (SpecialKey KeyRight) -> Just $ changeCursor cg CRight gs
  _                     -> Nothing 

capPos :: (Ord t1, Ord t) => ((t, t1), (t, t1)) -> (t, t1) -> (t, t1)
capPos ((x0,y0), (x1,y1)) (x,y) = (cap x x0 x1, cap y y0 y1)
  where
    cap n low hi | n < low = low
                 | n > hi  = hi
                 | otherwise = n

sunCursor, blockCursor, storeCursor :: GS -> (Int, Int) 
sunCursor gs   = cursors gs M.! SunCursor
blockCursor gs = cursors gs M.! BlockCursor
storeCursor gs = cursors gs M.! StoreCursor

drawSuns ::  GS -> Picture
drawSuns gs = Pictures [ drawSpritesAt posns sprs (cycle [Nothing]) miniblockSize
                       , drawCursor (sunCursor gs) spriteSize
                       ]
  where 
   sprs = fuSprites ++ fdSprites
   fuSprites = map ((`findSpriteOrError` sprites gs) . nameOfSprite) fuSuns
   fdSprites = replicate (length fdSuns) (findSpriteOrError "Sun FaceDown" (sprites gs))
   (fuSuns, fdSuns) = suns $ currentOrAuctionCurrentPlayer $ raBoard gs
   miniblockSize = 8
   spriteSize = 8 * miniblockSize
   posns = map (\x -> (x,0)) [0..]


findSpriteOrError ::  String -> Map String a -> a
findSpriteOrError k smap = fromMaybe 
                             (error $ "ERROR: No sprite by name "++k)
                             (M.lookup k smap)

spriteForTile :: SpriteMap -> Tile -> Maybe MySprite
spriteForTile smap t = M.lookup (nameOfSprite t) smap

boardConstraints ::  ((Integer, Integer), (Integer, Integer))
boardConstraints = ((0,0), (7,7))

data CursorDir = CLeft | CRight | CDown | CUp deriving (Eq, Show)

changePos :: CursorDir -> (Int, Int) -> (Int, Int)
changePos CLeft (x,y)  = (x-1, y)
changePos CRight (x,y) = (x+1, y)
changePos CDown (x,y)  = (x, y-1)
changePos CUp (x,y)    = (x, y+1)

type SpriteMap = Map String MySprite

data GS = GS { frame :: Int
             , cursorPos :: (Int, Int)
             , cursors :: Map CursorType (Int, Int)
             , raBoard :: Board
             , curSprite :: MySprite
             , sprites :: SpriteMap
             } deriving (Show, Eq)

drawSunOnBlock :: GS -> Picture
drawSunOnBlock gs = drawSprite cubeSize (findSpriteOrError (nameOfSprite s) (sprites gs)) Nothing
  where s = boardSun $ raBoard gs
        cubeSize = 8

drawBlock ::  GS -> Picture
drawBlock gs = Pictures [ drawSpritesAt posns sprs (cycle [Nothing]) blockSize
                        , drawCursor (blockCursor gs) spriteSize
                        ]
  where posns = [(x,0) | x <- [0.. length sprs]]
        sprs = map ((`findSpriteOrError` sprites gs) . nameOfSprite) $ block $ raBoard gs
        spriteSize = blockSize * 8 
        blockSize = 8

-- TODO: draw lightened placeholder tiles (or translucent), when not possessed
drawStore ::  GS -> Picture
drawStore gs = Pictures [ drawSpritesAt posns sprs countMs blockSize 
                        , drawCursor (storeCursor gs) spriteSize
                        ]
  where 
        (ts, posns, countMs) = storeOrWonTilesAtPositions gs
        blockSize = 8
        spriteSize = blockSize * 8
        sprs = map spriteFor ts
          where
            spriteFor (t,True) = findSpriteOrError (nameOfSprite t) (sprites gs)
            spriteFor (_t,False) = findSpriteOrError "PlaceHolder" (sprites gs) 

storeOrWonTilesAtPositions :: GS -> ([(StoreableTile, Bool)], [(Int, Int)], [Maybe Int])
storeOrWonTilesAtPositions gs = (ts, posns, countMs)
  where
     posns = [(x,y) | y <- [1,0], x <- [0.. rowWidth - 1]]
     rowWidth = length (head boardLayout)
     countMs = map count ts
       where
         count (t,_) = if c > 1 then Just c else Nothing 
           where c = length $ filter (==t) tilesInHandOrBeingWon

     ts                    = zip possibles $ map (`elem` tilesInHandOrBeingWon ) possibles
     tilesInHandOrBeingWon = 
        sort $ case gameMode $ raBoard gs of
         -- although when godding we only pick DR from store, (not store+block), 
         -- we still look at here rather than store, as this is updated during DR selection.
          (ResolveDisasters (sts,_,_) _ _) -> sts
          _                                -> tiles $ currentOrAuctionCurrentPlayer $ raBoard gs
     possibles = concat boardLayout

drawRaTrack :: GS -> Picture
drawRaTrack gs = drawSpritesAt posns sprs (cycle [Nothing]) 8
  where 
    posns = [(x, 0) | x <- [0 .. maxRC ] ]
    sprs = raSprs ++ placeHolderSprs
    raSprs = replicate rc (findSpriteOrError "Ra" $sprites gs)
    placeHolderSprs = replicate (maxRC - rc) (findSpriteOrError "PlaceHolder" $sprites gs)
    rc = raCount (raBoard gs)
    maxRC = raCountMax (raBoard gs)


drawScoring :: GS -> Picture
drawScoring gs = case gameMode $ raBoard gs of
  ShowScoring ep scrMap -> Pictures [ Color white $ rectangleSolid 800 500
                                    , translate (-400) (250)  $ drawTextLines black scoringText
                                    ]
                    where scoringText =  (concatMap snd $ M.elems scrMap) ++
                                         [ "At end of " ++ show ep
                                         , "Press 'c' to continue."]
  _           -> Blank

drawState :: GS -> Picture
drawState gs = Pictures 
   [ translate (200)  (100)  $ drawSprite 9 (curSprite gs) Nothing
   , translate (-300) (100)  $ drawRaTrack gs
   , translate (-300) (0)    $ drawSunOnBlock gs
   , translate (-200) (0)    $ drawBlock gs
   , translate (100)  (-150) $ drawSuns gs
   , translate (-300) (-300) $ drawStore gs
   , translate (0)    (0)    $ drawScoring gs
   , translate (-400) (-30)  $ drawTextLines colorSeaGlass messages]
  where 
    i = frame gs
    messages = [ titleForGameMode (gameMode $ raBoard gs)
               ] ++ map show (handlersForLegalAcs gs) ++
               [-- , "Deck: " ++ show $ deck $ raBoard gs
                "Frame: " ++ show i
                , show (epoch $ raBoard gs)
                , show (currentOrAuctionCurrentPlayerNum $ raBoard gs)

                , "Cursors: " ++ show (cursors gs)
               ]

titleForGameMode ::  GameMode -> String
titleForGameMode (InAuction{})            = "Auction!"
titleForGameMode (UsingGod _)             = "Use a God"
titleForGameMode (ShowScoring ep _scrMap) = "Scoring at end of " ++ show ep
titleForGameMode (ResolveDisasters{})     = "Resolve disaster(s)"
titleForGameMode (StartTurn)              = "Choose Action"


vecadd ::  (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
vecadd (x,y) (a,b) = (x+a, y+b)

drawSpritesAt ::  [(Int, Int)] -> [MySprite] -> [Maybe Int] -> Int -> Picture
drawSpritesAt posns sprs countMs sz = 
  Pictures $ map (\((x,y),s,n) -> 
    translate (fi x * sprSize) (fi y * sprSize) $ drawSprite sz s n) $ zip3 posns sprs countMs
    where 
      sprSize = fromIntegral sz * 8
      fi = fromIntegral

drawSprite :: Int -> MySprite -> Maybe Int -> Picture
drawSprite cubeSize (_sprName, spr) numM = 
  Pictures $ [ translate (-halfWid) (-halfWid) $ cubeAt ((x,y), c) cubeSize id -- (light . light) 
             | x <- [0..7], y <-[0..7], let c = spr ! (x,y) ] 
             ++ maybeToList numberOverlay
             -- ++ [_border, _marker]
    where
       numberOverlay = fmap numberOverlayFor numM
       numberOverlayFor n = translate quartWid 0 $ Pictures  [Scale 0.15 0.15 $ Color black $ Text (show n)
                                , translate (-2) 2 $ Scale 0.15 0.15 $ Color red $ Text (show n)] -- TODO: this could be coloured dots in border? Must be able to indicate double figures
       _border = Color green $ rectangleWire wholeSpriteSize wholeSpriteSize
       _marker = Color green $ rectangleSolid 1 1
       halfWid = wholeSpriteSize / 2
       quartWid = wholeSpriteSize / 4
       wholeSpriteSize = 8 * fromIntegral cubeSize

type ColorEffect = (Color -> Color)

cubeAt :: ((Int, Int), Int) -> Int -> ColorEffect -> Picture
cubeAt ((x,y),cIx) size effect = case cIx of
      0 -> Pictures []  
      _ -> translate (f x) (f y) $ cubeSolid c edgeLen
            where c = effect $ colorFor cIx
                  f n = fromIntegral size * ((0.5::Float) + fromIntegral n)
                  edgeLen = round $ fromIntegral size * (0.9::Float)

cubeSolid ::  Color -> Int -> Picture
cubeSolid c w =  Pictures 
    [ Color black $ rectangleSolid (f w) (f w)
    , Color c $ rectangleSolid (f w2) (f w2) 
    ]
  where f  = fromIntegral
        w2 | w < 8 = w - 1
           | otherwise = w - 4

drawCursor ::  (Int, Int) -> Int -> Picture
drawCursor (x,y) sz = Color yellow $ translate (m x) (m y) $ rectangleWire (fi sz) (fi sz)
  where m = fi .  (sz *)
        fi = fromIntegral

colorSea ::  Color
colorSea = makeColor8 46 90 107 255

colorSeaGlass ::  Color
colorSeaGlass = makeColor8 163 204 188 255

drawTextLines :: Color -> [String] -> Picture
drawTextLines c ls = Color c $
                   Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,row) = textAt 10 (row*(-20)) l

textAt ::  Float -> Float -> String -> Picture
textAt x y content = Translate x y (Scale 0.12 0.12 (Text content))

textWithSprites :: SpriteMap -> String -> Picture
textWithSprites sprMap msg = drawSpritesAt posns sprs (cycle [Nothing]) 8
  where
    posns = zip [0..] (cycle [0])
    sprs = mapMaybe ((`M.lookup` sprMap) . (:"")) msg

readCharsMap :: [String] -> M.Map String MySprite
readCharsMap = M.fromList . readChars

readChars :: [String] -> [(String, MySprite)]
readChars [] = []
readChars ls = thing : readChars remainder
  where (thing, remainder) = (exName $ readChar $ take 9 ls, drop 9 ls)
        exName (n,r) = (n, (n,r))

readChar :: [String] -> MySprite
readChar ls = case ls of
  (name:rest) -> (name, toArr $ map readCharLine rest)
  other       -> error $ "bad char data" ++ unlines other
  where
    toArr = array ((0,0), (7,7)) . zip pts . concat 
    pts = [(x,y) | y <- [7,6..0], x <- [0..7]]

readCharLine :: String -> [Int]
readCharLine = map (read . (:""))

spriteName ::  (t, t1) -> t
spriteName (n,_) = n

spriteArray ::  (t, t1) -> t1
spriteArray (_, ar) = ar

class HasSpriteName a where
  nameOfSprite :: a -> String

instance HasSpriteName Tile where
  nameOfSprite (Storeable st)     = nameOfSprite st
  nameOfSprite (NonStoreable nst) = nameOfSprite nst

instance HasSpriteName NonStoreableTile where
  nameOfSprite = show
instance HasSpriteName StoreableTile where
  nameOfSprite = show
instance HasSpriteName Sun where
  nameOfSprite (Sun x) = "Sun " ++ show x

boardLayout :: [[StoreableTile]]
boardLayout = [ topMons ++ topCivs ++ [Pharaoh] ++ [God] ++ [Flood]
              , lowMons ++ lowCivs ++ [Gold] ++ [Nile]
              ]
  where 
    (topMons, lowMons) = splitCounted 4 mons
    (topCivs, lowCivs) = splitCounted 2 civs
    mons = map Monument allMonumentTypes
    civs = map Civilization allCivilizationTypes
    splitCounted n xs = (take n xs, drop n xs)

