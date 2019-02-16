{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Defines types of picutres in game.
data Tile = Wall | Ground | Storage | Box | Blank | Player deriving (Eq)

data Direction = R | U | L | D

data Coord = C Integer Integer

-- Gives values to pictures.
wall, ground, storage, box, player :: Picture
wall    = colored (grey 0.4) (solidRectangle 1 1)
ground  = colored yellow (solidRectangle 1 1)
box     = colored brown (solidRectangle 1 1)
player  = arrow & (colored purple (solidCircle 0.3)) & ground
storage = solidCircle 0.3 &  ground

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank
drawTile Player  = player

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c =
  translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))

initialCoord :: Coord
initialCoord = C 0 1

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

nextCoord :: Direction -> Coord -> Coord
nextCoord d (C x y)
  | maze adj == Ground = adj
  | maze adj == Storage = adj
  | otherwise = (C x y)    
      where adj = adjacentCoord d (C x y)
    
handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = nextCoord R c
    | key == "Up"    = nextCoord U c
    | key == "Left"  = nextCoord L c
    | key == "Down"  = nextCoord D c
    | otherwise      = c
handleEvent _ c      = c

arrow :: Picture
arrow = translated (0.5) 0 $ rotated (pi/2) $ (left & right)
          where left = rotated (pi/10) $ path [(0,0),(0,1)]
                right = rotated (-pi/10) $ path [(0,0),(0,1)]

drawState :: Coord -> Picture
drawState c = (drawPlayer c) & pictureOfMaze

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
  
drawPlayer :: Coord -> Picture
drawPlayer (C x y) =
  if x == 0 && y == 0 then atCoord (C 0 1) (drawTile Player)
  else atCoord (C x y) $ drawTile Player

maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                 = Ground

exercise1 :: IO()
exercise1 = interactionOf initialCoord handleTime handleEvent drawState

main :: IO ()
main = exercise1
