isPrime :: Integral a => a -> Bool
isPrime 1 = True
isPrime x = and [if x `mod` y == 0 then False else True| y <- [2 .. x - 1]]

primeList :: Integral a => a -> [a]
primeList x = [y | y <- [1 .. x - 1], isPrime y]

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

filter' p []                 = []
filter' p (x:xs) | p x       = x:filter' p xs
                 | otherwise = filter' p xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0
