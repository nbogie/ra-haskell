module Main where
import Data.Array
import Data.List(find)
import Data.Map (Map)
import Data.Maybe(fromMaybe,mapMaybe)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Prelude hiding (pi)
import System.Environment(getArgs)
import qualified Data.Map as M
import Data.Char (ord, chr)

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
   , cursorPos = (0,0)
   , curSprite = initSprite
   , sprites = sprs}

initSprite ::  (String, Array (Int, Int) Int)
initSprite = ("untitled", array ((0,0), (7,7)) [((x,y),0) | x<- [0..7], y <- [0..7]])

backgroundColor ::  Color
backgroundColor = colorSea 


data Input = InKey Char | CursorSelection CursorGroup deriving (Show, Eq)
data CursorGroup = CSGoddableTiles | CSDisasters | CSDiscardables | CSSuns deriving (Show, Eq)
inpFor :: Action -> Input
inpFor DrawTile                = InKey 'd'
inpFor (CallRa _)              = InKey 'r'
inpFor UseGod                  = InKey 'g'
inpFor PickTileToGod           = CursorSelection CSGoddableTiles
inpFor AcUseAnotherGod           = InKey 'y'
inpFor FinishWithGods          = InKey 'n'
inpFor ChooseDisasterToResolve = CursorSelection CSDisasters
inpFor ChooseTilesToDiscard    = CursorSelection CSDiscardables
inpFor (BidASun _pi sun)       = InKey $ chr $ ord 'a' + (sunValue sun) - 1
inpFor Pass                    = InKey 'p'

handleInput :: Event -> GS -> IO GS
handleInput e gs = case chosenActionM of
  Just (a, _) -> return $ traceShow a $ applyAction a gs
  Nothing -> return gs
  where 
   chosenActionM = find (eventMatches e . snd) (inputsForLegalAcs gs)

inputsForLegalAcs ::  GS -> [(Action, Input)]
inputsForLegalAcs gs = zip legalAcs (map inpFor legalAcs)
 where
   mode = gameMode $ raBoard gs
   legalAcs = legalActions (raBoard gs) mode

eventMatches ::  Event -> Input -> Bool
eventMatches (EventKey (Char cE) Down _mods _) (InKey cI) = cE == cI
eventMatches _ _ = False


applyAction ::  Action -> GS -> GS
applyAction ac  gs = gs { raBoard = newBoard } 
  where newBoard = apply ac $ raBoard gs 

handleCursor ::  Key -> t -> GS -> IO GS
handleCursor k _mods gs  = 
  case k of
  (SpecialKey KeyDown)  -> changeCursor CDown gs
  (SpecialKey KeyUp)    -> changeCursor CUp gs
  (SpecialKey KeyLeft)  -> changeCursor CLeft gs
  (SpecialKey KeyRight) -> changeCursor CRight gs
  _                     -> return gs

changeCursor :: CursorDir -> GS -> IO GS
changeCursor d  gs = return $ gs { cursorPos = capPos ((0,0), (7,7)) $ changePos d $ cursorPos gs } 

capPos :: (Ord t1, Ord t) => ((t, t1), (t, t1)) -> (t, t1) -> (t, t1)
capPos ((x0,y0), (x1,y1)) (x,y) = (cap x x0 x1, cap y y0 y1)
  where
    cap n low hi | n < low = low
                 | n > hi  = hi
                 | otherwise = n

drawSuns ::  GS -> Picture
drawSuns gs = drawSpritesAt posns sprs spritesSize
  where 
   sprs = fuSprites ++ fdSprites
   fuSprites = map ((`findSpriteOrError` sprites gs) . nameOfSprite) fuSuns
   fdSprites = replicate (length fdSuns) (findSpriteOrError "Sun FaceDown" (sprites gs))
   (fuSuns, fdSuns) = suns $ active $ raBoard gs
   spritesSize = 8
   posns = map (\x -> (x,0)) [0..]

findSpriteOrError ::  String -> Map String a -> a
findSpriteOrError k smap = fromMaybe 
                             (error $ "ERROR: No sprite by name "++k)
                             (M.lookup k smap)

drawTile ::  GS -> GS
drawTile gs = gs { raBoard = newBoard
                 , curSprite = fromMaybe cSprite newSprite } 
  where newBoard = (raBoard gs) { deck = newDeck }
        cSprite = curSprite gs
        topTileM :: Maybe Tile
        newDeck :: [Tile]
        (topTileM, newDeck) = popOrStay oldDeck
        oldDeck = deck $ raBoard gs
        popOrStay :: [Tile] -> (Maybe Tile, [Tile])
        popOrStay [] = (Nothing, [])
        popOrStay (x:xs) = (Just x, xs)
        newSprite = topTileM >>= spriteForTile (sprites gs)

spriteForTile :: SpriteMap -> Tile -> Maybe MySprite
spriteForTile smap t = M.lookup (nameOfSprite t) smap

boardConstraints ::  ((Integer, Integer), (Integer, Integer))
boardConstraints = ((0,0), (7,7))

data CursorDir = CLeft | CRight | CDown | CUp deriving (Eq, Show)

changePos ::  (Num t1, Num t) => CursorDir -> (t, t1) -> (t, t1)
changePos CLeft (x,y)  = (x-1, y)
changePos CRight (x,y) = (x+1, y)
changePos CDown (x,y)  = (x, y-1)
changePos CUp (x,y)    = (x, y+1)

type SpriteMap = Map String MySprite

data GS = GS { frame :: Int
             , cursorPos :: (Int, Int)
             , raBoard :: Board
             , curSprite :: MySprite
             , sprites :: SpriteMap
             } deriving (Show, Eq)

drawBlock ::  GS -> Picture
drawBlock gs = drawSpritesAt posns sprs 8
  where posns = [(x,0) | x <- [0.. length sprs]]
        sprs = map ((`findSpriteOrError` sprites gs) . nameOfSprite) $ block $ raBoard gs

-- TODO: draw lightened placeholder tiles (or translucent), when not possessed
drawStore ::  GS -> Picture
drawStore gs = drawSpritesAt posns sprs 8
  where posns = [(x,y) | y <- [1,0], x <- [0.. rowWidth - 1]]
        rowWidth = length (head boardLayout)
        sprs = map sname ts
        sname (t,True) = findSpriteOrError (nameOfSprite t) (sprites gs)
        sname (_t,False) = findSpriteOrError "PlaceHolder" (sprites gs) 
        ts = zip possibles $ map (`elem` (tiles $ active $ raBoard gs)) possibles
        possibles = concat boardLayout

drawRaTrack :: GS -> Picture
drawRaTrack gs = drawSpritesAt posns sprs 8
  where 
    posns = [(x, 0) | x <- [0 .. maxRC ] ]
    sprs = raSprs ++ placeHolderSprs
    raSprs = replicate rc (findSpriteOrError "Ra" $sprites gs)
    placeHolderSprs = replicate (maxRC - rc) (findSpriteOrError "PlaceHolder" $sprites gs)
    rc = raCount (raBoard gs)
    maxRC = raCountMax (raBoard gs)


drawState :: GS -> Picture
drawState gs = Pictures 
   [ translate (200)  (100)  $ drawSprite 9 (curSprite gs)
   , translate (-300) (100)  $ drawRaTrack gs
   , translate (-200) (0)    $ drawBlock gs
   , translate (100)  (-150) $ drawSuns gs
   , translate (-300) (-300) $ drawStore gs
   , translate (-400) (-30) $ drawTextLines colorSeaGlass messages]
  where 
    i = frame gs
    messages = [ "Ra Sprite GUI"
               , "Game Mode: " ++ show (gameMode $ raBoard gs)
               ] ++ map show (inputsForLegalAcs gs) ++
               [-- , "Deck: " ++ show $ deck $ raBoard gs
                "Frame: " ++ show i
               ]

vecadd ::  (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
vecadd (x,y) (a,b) = (x+a, y+b)

drawSpritesAt ::  [(Int, Int)] -> [MySprite] -> Int -> Picture
drawSpritesAt posns sprs sz = 
  Pictures $ map (\((x,y),s) -> 
    translate ((fi x) * sprSize) ((fi y) * sprSize) $ drawSprite sz s) $ zip posns sprs
    where 
      sprSize = fromIntegral sz * 8
      fi = fromIntegral

drawSprite :: Int -> MySprite -> Picture
drawSprite cubeSize (_sprName, spr) = 
  Pictures $ [ translate (-halfWid) (-halfWid) $ cubeAt ((x,y), c) cubeSize id -- (light . light) 
             | x <- [0..7], y <-[0..7], let c = spr ! (x,y) ] 
             -- ++ [_border, _marker]
    where
       _border = Color green $ rectangleWire wholeSpriteSize wholeSpriteSize
       _marker = Color green $ rectangleSolid 1 1
       halfWid = wholeSpriteSize / 2
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

drawCursor ::  Integral b => (b, b) -> b -> Picture
drawCursor (x,y) sz = Color yellow $ translate (m x) (m y) $ rectangleWire (fi sz) (fi sz)
  where m = fi .  (sz *)
        fi = fromIntegral

colorSea ::  Color
colorSea      = makeColor8 46 90 107 255
colorSeaGlass ::  Color
colorSeaGlass = makeColor8 163 204 188 255

drawTextLines :: Color -> [String] -> Picture
drawTextLines c ls = Color c $
                   Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,row) = textAt 10 (row*(-20)) l

textAt ::  Float -> Float -> String -> Picture
textAt x y content = Translate x y (Scale 0.12 0.12 (Text content))

textWithSprites :: SpriteMap -> String -> Picture
textWithSprites sprMap msg = drawSpritesAt posns sprs 8
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

writeSprites  :: SpriteMap -> IO () 
writeSprites sprMap = do
  let content = concatMap writeSprite (M.toList sprMap)
  writeFile "sprites.dat" content
  
writeSprite :: (String, MySprite) -> String
writeSprite (name, (_, arry)) = unlines $ name : write arry
   where
   write :: GridArray -> [String]
   write ar = wrapAt 8 [head $ show c | y <- [7,6..0], x <- [0..7], let c = ar ! (x,y)]
   wrapAt _ [] = []
   wrapAt n xs = take n xs : wrapAt n (drop n xs)

spriteName ::  (t, t1) -> t
spriteName (n,_) = n

spriteArray ::  (t, t1) -> t1
spriteArray (_, ar) = ar

class HasSpriteName a where
  nameOfSprite :: a -> String

instance HasSpriteName Tile where
  nameOfSprite (Storeable st) = nameOfSprite st
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


