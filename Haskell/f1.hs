module F1 where

import System.Environment
import Data.Char
import Data.List

ackfib :: [Integer] -> [Integer]
ackfib [x,y] = [x] ++ ackfib [y,x+y]

fib :: Integer -> Integer
fib n = last $ take (fromInteger n+1) $ ackfib [0,1]

vowels = "aouieyåäöAOUIEYÅÄÖ"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

isConsonant :: Char -> Bool
isConsonant x = not (isVowel x) && isAlpha x

pirateSpeech :: Char -> String
pirateSpeech x | isConsonant x = [x, 'o', x]
			   | otherwise = [x]

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak xs = concat $ map pirateSpeech xs

karpsravor :: String -> String
karpsravor [x] = [x]
karpsravor [x,y] = [x,y]
karpsravor (x:y:z:xs) = 
	if isConsonant x && ([x,y,z] == [x,'o',x])
	then x : karpsravor(xs) 
	else x : karpsravor( (y:z:xs) )
karpsravor [] = []

numAlpha :: String -> Int
numAlpha s = length $ filter isAlpha s

nWords :: String -> Integer -> Integer
nWords [] count = count
nWords [x] count 
		| isAlpha x = count + 1
		| otherwise = count
nWords (x:y:xs) count 
		| (not $ isAlpha y) && (isAlpha x) = nWords xs count+1
		| otherwise = nWords (y:xs) count

medellangd :: String -> Double
medellangd [] = 0.0
medellangd s = (realToFrac $ numAlpha s) / (realToFrac $ nWords s 0)

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