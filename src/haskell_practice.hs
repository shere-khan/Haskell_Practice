import Data.List
import qualified Data.Map as Map

-- data Rank = Ace | Two | Three | Four | Five | Siz | Seven
	    -- | Eight | Nine | Ten | Jack | Queen | King 
	    -- deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- data Suit = Spades | Hears | Diamonds | Clubs
	    -- deriving (Eq, Enum, Show, Read)

-- data Card = Card Rank Suit deriving (Eq, Show, Read)

-- type Hand = [Card]

-- digitSum :: Int -> Int
-- digitSum x = 

-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v):xs) | key == k = Just v
			-- | otherwise findKey key xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2340")
    ,("bonnie", "348-3948"), ("patsy", "349-3480"), 
    ("lucille", "984-8374"), ("wendy", "393-9785"), ("penny", "394-0987")]

-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- findKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

mergeSort :: (Num a) => (a-> a-> Bool) -> [a] -> [a]
mergeSort p xs = 
       let middle = (length xs) `div` 2
	   getFstHalf = fst . splitAt middle
       	   getSndHalf = snd . splitAt middle
       in if length xs == 1 then xs 
          else merge p (mergeSort p $ getFstHalf xs) (mergeSort p $ getSndHalf xs)

merge :: (Num a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
-- merge :: (Ord a) => [a] -> [a] -> [a]
merge p [] [] = []
merge p x [] = x
merge p [] y = y
merge p (x:xs) (y:ys) | p x y = x : merge p xs (y:ys)
                    | otherwise = y : merge p (x:xs) ys

main :: IO ()
main = do
    putStrLn "Hello world"
    --show . merge [1, 3] [2, 4]
