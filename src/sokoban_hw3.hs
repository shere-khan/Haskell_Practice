{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-deprecations #-}
import CodeWorld

------------------------------------ LISTS ----------------------------------
data List a = Empty | Entry a (List a) deriving (Show)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

------------------------------------ COORDINATES ----------------------------------
data Coord = C Integer Integer deriving (Show)

data Direction = R | U | L | D

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

------------------------------------ THE MAZE ----------------------------------
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

noBoxMaze :: Coord -> Tile
noBoxMaze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | otherwise                = Ground
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes l (C x y)
  | containsCoord l (C x y) = Box
  | otherwise               = noBoxMaze (C x y)

containsCoord :: List Coord -> Coord -> Bool
containsCoord Empty _ = False 
containsCoord (Entry (C z w) k) (C x y)
  | z == x && w == y = True
  | otherwise        = False || containsCoord k (C x y)

data State = State Coord Direction (List Coord)

------------------------ GET COORDINATES OF BOXES -----------------------------
recurseCol :: Integer -> Integer -> List Coord
recurseCol _ 10 = Empty
recurseCol 10 _ = Empty
recurseCol x y = case maze (C x y) of
                   Box -> (Entry (C x y) next)
                   t   -> next 
  where next = recurseCol x (y+1)

recurseRow :: Integer -> Integer -> List Coord
recurseRow _ 10 = Empty
recurseRow 10 _ = Empty
recurseRow x y  = appendList (recurseCol x y) (recurseRow (x + 1) y)

appendList :: List Coord -> List Coord -> List Coord
appendList k Empty = k
appendList Empty k = k
appendList (Entry a k) (Entry b l) = (Entry a (Entry b (appendList k l)))

---------------------------------- EVENT HANDLING ----------------------------------
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (State c d l)
    | key == "Right" = calcMove (State c R l)
    | key == "Left"  = calcMove (State c L l)
    | key == "Up"    = calcMove (State c U l)
    | key == "Down"  = calcMove (State c D l)
handleEvent _ s = s

calcMove :: State -> State
calcMove (State c d l) = case mazeWithBoxes l to of
                           Box     -> calcBoxMove (State c d l)
                           Ground  -> (State to d l)
                           Storage -> (State to d l)
                           Wall    -> (State c d l)
  where to = adjacentCoord d c

calcBoxMove :: State -> State
calcBoxMove (State c d l)
      | canBoxMove (State from d l) = (State from d (moveBox l from to))
      | otherwise                   = (State c    d l)
    where from = adjacentCoord d c
          to   = adjacentCoord d from

canBoxMove :: State -> Bool
canBoxMove (State from d l) = case mazeWithBoxes l to of
               Box     -> False
               Ground  -> True
               Storage -> True
               Wall    -> False
  where to = adjacentCoord d from

moveBox :: List Coord -> Coord -> Coord -> List Coord
moveBox Empty _ _ = Empty
moveBox (Entry a l) from to
  | eqCoord a from = (Entry to l)
  | otherwise      = (Entry a (moveBox l from to))

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x y) (C z w) = if x == z && y == w then True else False

------------------------------------ DRAWING ----------------------------------
wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white      (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

drawTileAt2 :: Coord -> Picture
drawTileAt2 c = atCoord c (drawTile (mazeWithBoxes initialBoxes c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(-0.3,0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,0),(-0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

initialBoxes :: List Coord
initialBoxes = recurseRow (-10) (-10)

initialState :: State
initialState = State (C 0 1) R initialBoxes

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (State c d l)
    | isWon (State c d l)   = (colored purple (solidCircle 4)) & pic
    | otherwise = pic
  where pic = pictureOfBoxes l & (atCoord c $ player d) & pictureOfMaze

----------------------- COMPLETE INTERACTION ----------------------------------
sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

----------------------- GENERAL INTERACTION TYPE ----------------------------------
data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

--------------------------- RESETABLE INTERACTIONS ----------------------------------
resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

------------------------------------ START SCREEN ----------------------------------
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

------------------------- WINNING GAME -------------------------
isWon :: State -> Bool
isWon (State c d (Entry a l)) = allList $ mapList isOnStorage l

isOnStorage :: Coord -> Bool
isOnStorage c
  | noBoxMaze c == Storage = True
  | otherwise              = False

allList :: List Bool -> Bool
allList Empty = True
allList (Entry a l) = a && allList l

------------------------- MAIN FUNCTION -------------------------
main :: IO ()
main = runInteraction $ resetable $ withStartScreen sokoban
--main = runInteraction sokoban
-- main = drawingOf pictureOfMaze
