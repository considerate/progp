module Hamming
(hammingDist)
where


different :: (Eq a) => (a, a) -> Bool
different (x,y) = x /= y

hammingDist :: (Eq a) => [a] -> [a] -> Int
hammingDist xs ys 
	| length xs == length ys = length $ filter different $ zip xs ys
	| otherwise = error "The lists need to be same length"
