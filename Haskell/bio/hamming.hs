module Hamming
(hammingDist)
where

hammingDist :: String -> String -> Int
hammingDist xs ys 
	| length xs == length ys = length $ filter (uncurry (/=)) (zip xs ys)
	| otherwise = error "The lists need to be same length"
