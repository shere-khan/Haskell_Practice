module Main where

increasing :: (Ord a) => [a] -> Bool
increasing xs = if xs == []
                then True
                else if tail xs == []
                     then True
                     else if head xs <= head (tail xs)
                          then increasing (tail xs)
                          else False
                          
increasing2 :: (Ord a) => [a] -> Bool
increasing2 [] = True
increasing2 [x] = True
increasing2 (x:y:ys) = x <= y && increasing (y:ys)

main::IO()
main = undefined