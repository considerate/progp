module F1 where

import Data.Char

fib :: Int -> Int
fib n = fibs !! n 
	where fibs = f 0 1 
		where f a b = a : f b (a+b)

vowels :: String
vowels = "oaieuy"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
	| isVowel x = x : rovarsprak xs
	| otherwise = x : 'o' : x : rovarsprak xs

karpsravor :: String -> String
karpsravor (x:y:z:xs)
	| isVowel x = x : karpsravor (y:z:xs)
	| otherwise = x : karpsravor xs
karpsravor xs = xs

divide :: (Int,Int) -> Double
divide (x,y) = realToFrac y / realToFrac x

wordCounts :: String -> (Int, Int) -> (Int,Int)
wordCounts "" (w,l) = (w,l)
wordCounts [x] (w,l)
	| isAlpha x = (w+1,l+1)
	| otherwise = (w,l)
wordCounts (x:y:xs) (w,l)
	| not (isAlpha y) && isAlpha x = wordCounts xs (w+1,l+1)
	| isAlpha x = wordCounts (y:xs) (w,l+1)
	| otherwise = wordCounts (y:xs) (w,l)

medellangd :: String -> Double
medellangd [] = 0.0
medellangd s =  divide $ wordCounts s (0,0)

oddsevens :: [a] -> ([a],[a])
oddsevens [] = ([],[])
oddsevens [x] = ([x],[])
oddsevens [x,y] = ([x],[y])
oddsevens (x:y:xs) = (x:odds, y:evens) where (odds,evens) = oddsevens xs

skyfflalists :: [a] -> [[a]]
skyfflalists [] = []
skyfflalists [x] = [[x]]
skyfflalists xs = odds : skyfflalists evens where (odds,evens) = oddsevens xs

skyffla :: [a] -> [a]
skyffla [] = []
skyffla xs =　concat $ skyfflalists xs