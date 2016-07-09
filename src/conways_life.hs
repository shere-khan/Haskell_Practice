import qualified Data.Map as Map
import System.Random as IRandom

main = do
    -- let mapList = [((0, 0), 0), ((0, 1), 1), ((0, 2), 2)]
    putStrLn "hello world"

data Point = Point Int Int deriving (Show, Ord)

data Board = Board [(Point, Int)] deriving (Show)

instance Eq Point where
    (Point x y) == (Point p q) = x == p && y == q

--data Board a = EmptyBoard | Board { val:: a, 
                                    --nw :: Board a, 
                                    --n :: Board a, 
                                    --ne :: Board a, 
                                    --e :: Board a, 
                                    --se :: Board a, 
                                    --s :: Board a, 
                                    --sw :: Board a, 
                                    --w :: Board a } deriving (Show)

-- mapList = Board [((Point 0 0), 0), ((Point 0 1), 1), ((Point 0 2), 2)]

constructBoard :: Int -> Board
constructBoard size = Board [(Point x y, 0) | x <- [0..size], y <- [0..size]]



randomBoolean :: Bool
randomBoolean = let getRandom :: (Int, StdGen)
                    getRandom = randomR (0, 100) (mkStdGen 394839)
                in if (fst getRandom) < 60 then False else True

mapList = constructBoard 8

findPoint :: Point -> Board -> Int
findPoint searchPoint (Board ((point, s):zs)) | searchPoint == point = s
                                              | otherwise            = findPoint searchPoint (Board zs)

