primes :: (Num a, Eq a) => a -> [a]
primes x | x == 0    = [0]
	     | x == 1    = [1]
		 | x == 2    = [2]
		 | x == 3    = [3]
		 | otherwise = map mod list
			where list = [y | y <- [4..x], isPrime y]

isPrime Num a => a -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = and [if x `mod` y == 0 then False else True| y <- [2..x]]

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
		   if n == 0 then 0 else 1
		   
-- Guarded equations
abs n | n >= 0     =  n
	  | otherwise  = -n

signum n | n < 0 = -1
		 | n == 0 = 0
		 | otherwise = 1

-- Pattern matching		 
not   :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
True && True = True

True && True = True
_ && _ = False

-- most optimal
True && b = b
False && _ = False

head :: [a] -> abs
head (x:_) = x

tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs

map (\x -> x + 5) [3, 4]