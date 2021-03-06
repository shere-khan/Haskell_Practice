import Data.Char

primes :: Integral a => a -> [a]
primes x  = [y | y <- [2..x], isPrime y]

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime x = and [if mod x y == 0 then False else True| y <- [2..(x - 1)]]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..length xs - 1])

let2int :: Char -> Int
let2int c
    | isLower c = ord c - ord 'a'
    | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c || isUpper c = int2let ((let2int c + n) `mod` 26 )
    | otherwise = c
	
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

x = "Think like a Fundamentalist Code like a Hacker"

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sum' = foldr' (+) 0

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- (.') :: (b -> c) -> (a -> b) -> (a -> c)
-- f .' g = (\x -> f (g x))

sumsqreven ns = sum . map (^2) . filter even

twice :: (b -> b) -> b -> b
twice f = f . f

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \inp -> [(v, inp)]

failure' :: Parser a
failure' = \inp -> []

item' :: Parser Char
item' = \inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)]

parse' :: Parser a-> String -> [(a, String)]
parse' p inp = p inp

(##) :: Parser a -> (a -> Parser b) -> Parser b
p ## f = \inp -> case parse' p inp of
                      [] -> []
                      [(v, out)] -> parse' (f v) out
