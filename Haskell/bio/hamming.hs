module Hamming
(hammingDist)
where

hammingDist :: String -> String -> Int
hammingDist xs ys 
	| length xs == length ys = length $ filter (\(x,y) -> x /= y) (zip xs ys)
	| otherwise = error "The lists need to be same length"
