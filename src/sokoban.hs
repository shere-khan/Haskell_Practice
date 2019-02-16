{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Defines types of picutres in game.
data Tile = Wall | Ground | Storage | Box | Blank | Player deriving (Eq)

data Direction = R | U | L | D | None

data Coord = C Integer Integer

data Orientation = O Direction Coord

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
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (O None (C r c))))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Orientation -> Picture
drawTileAt (O d (C r c)) =
  translated (fromIntegral r) (fromIntegral c) (drawTile (maze (O d (C r c))))

initialCoord :: Orientation
initialCoord = O R (C 0 1)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Orientation -> Orientation
adjacentCoord (O R (C x y)) = (O R (C (x+1) y))
adjacentCoord (O U (C x y)) = (O U (C x (y+1)))
adjacentCoord (O L (C x y)) = (O L (C (x-1) y))
adjacentCoord (O D (C x y)) = (O D (C x (y-1)))

handleTime :: Double -> Orientation -> Orientation
handleTime _ o = o

nextCoord :: Orientation -> Orientation
nextCoord o
  | maze adj == Ground = adj
  | maze adj == Storage = adj
  | otherwise = o
      where adj = adjacentCoord o
      
handleEvent :: Event -> Orientation -> Orientation
handleEvent (KeyPress key) o
    | key == "Right"  = nextCoord o
    | key == "Up"     = nextCoord o
    | key == "Left"   = nextCoord o
    | key == "Down"   = nextCoord o
    | otherwise       = o
handleEvent _ o       = o

arrow :: Picture
arrow = translated (0.5) 0 $ rotated (pi/2) $ (left & right)
          where left = rotated (pi/10) $ path [(0,0),(0,1)]
                right = rotated (-pi/10) $ path [(0,0),(0,1)]

drawState :: Orientation -> Picture
drawState o = (drawPlayer o) & pictureOfMaze
  
drawPlayer :: Orientation -> Picture
drawPlayer (O d (C x y)) =
  if x == 0 && y == 0 then atCoord (C 0 1) (drawTile Player)
  else atCoord (C x y) $ drawTile Player

maze :: Orientation -> Tile 
maze (O d (C x y))
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
