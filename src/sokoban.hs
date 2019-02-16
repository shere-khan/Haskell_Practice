{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Defines types of picutres in game.
data Tile = Wall | Ground | Storage | Box | Blank |
	UpPlayer | DownPlayer | LeftPlayer | RightPlayer deriving (Eq)

data Direction = R | U | L | D | None deriving (Eq)

data Coord = C Integer Integer

data Orientation = O Direction Coord

-- Gives values to pictures.
wall, ground, storage, box :: Picture
wall         = colored (grey 0.4) (solidRectangle 1 1)
ground       = colored yellow (solidRectangle 1 1)
box          = colored brown (solidRectangle 1 1)
player       = arrow & (colored purple (solidCircle 0.3)) & ground
upPlayer     = rotated (pi/2) player
downPlayer   = rotated (pi) player
downPlayer   = rotated (3*pi/2) player
rightPlayer  = player
storage      = solidCircle 0.3 &  ground

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank
drawTile UpPlayer  = upPlayer
drawTile DownPlayer = downPlayer
drawTile LeftPlayer = leftPlayer
drawTile RightPlayer = rightPlayer

arrow :: Picture
arrow = translated (0.5) 0 $ rotated (pi/2) $ (piece1 & piece2)
          where piece1 = rotated (pi/10) $ path [(0,0),(0,1)]
                piece2 = rotated (-pi/10) $ path [(0,0),(0,1)]

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

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Orientation -> Orientation
adjacentCoord (O R (C x y)) = (O R (C (x+1) y))
adjacentCoord (O U (C x y)) = (O U (C x (y+1)))
adjacentCoord (O L (C x y)) = (O L (C (x-1) y))
adjacentCoord (O D (C x y)) = (O D (C x (y-1)))

nextCoord :: Orientation -> Orientation
nextCoord o
  | maze adj == Ground = adj
  | maze adj == Storage = adj
  | otherwise = o
      where adj = adjacentCoord o

drawState :: Orientation -> Picture
drawState o = (drawPlayer o) & pictureOfMaze
  
drawPlayer :: Orientation -> Picture
drawPlayer (O d (C x y))
	| d == U = atCoord (C x y) $ drawTile UpPlayer
	| d == L = atCoord (C x y) $ drawTile LeftPlayer
	| d == D = atCoord (C x y) $ drawTile DownPlayer
	| d == R = atCoord (C x y) $ drawTile RightPlayer
	| otherwise = atCoord (C x y) $ drawTile UpPlayer


maze :: Orientation -> Tile 
maze (O d (C x y))
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                 = Ground

initialCoord :: Orientation
initialCoord = O R (C 0 1)

exercise1 :: IO()
exercise1 = interactionOf initialCoord handleTime handleEvent drawState

------------------ EVENT HANDLING ---------------------------------------
handleEvent :: Event -> Orientation -> Orientation
handleEvent (KeyPress key) (O d c) 
    | key == "Right"  = nextCoord (O R c)
    | key == "Up"     = nextCoord (O U c)
    | key == "Left"   = nextCoord (O L c)
    | key == "Down"   = nextCoord (O D c)
    | otherwise       = (O d c)
handleEvent _ (O d c)       = (O d c)

handleTime :: Double -> Orientation -> Orientation
handleTime _ o = o

------------------ MAIN ---------------------------------------
main :: IO ()
main = exercise1
