module F1 where

import Data.Char
import Data.List

ackfib :: [Integer] -> [Integer]
ackfib [x,y] = x : ackfib [y,x+y]

fib :: Integer -> Integer
fib n = last $ take (fromInteger n+1) $ ackfib [0,1]

vowels = "aouiey"

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
divide (x,y) = (realToFrac y) / (realToFrac x)

wordCounts :: String -> (Int, Int) -> (Int,Int)
wordCounts "" (w,l) = (w,l)
wordCounts [x] (w,l)
	| isAlpha x = (w+1,l+1)
	| otherwise = (w,l)
wordCounts (x:y:xs) (w,l)
	| (isAlpha x) && (not $ isAlpha y) = wordCounts xs (w+1,l+1)
	| isAlpha x = wordCounts (y:xs) (w,l+1)
	| otherwise = wordCounts (y:xs) (w,l)

medellangd :: String -> Double
medellangd [] = 0.0
medellangd s =  divide $ wordCounts s (0,0)

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds [x,y] = [x]
odds (x:y:xs) = x : odds xs

evens :: [a] -> [a]
evens [] = []
evens [x] = []
evens [x,y] = [y]
evens (x:y:xs) = y : evens xs

skyfflalists :: [a] -> [[a]]
skyfflalists [] = []
skyfflalists [x] = [[x]]
skyfflalists xs = odds xs : (skyfflalists $ evens xs)

skyffla :: [a] -> [a]
skyffla [] = []
skyffla xs = concat $ skyfflalists xs