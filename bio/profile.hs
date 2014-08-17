module Profile
(Profile)
where

import MolSeq

import Data.Packed.Matrix
import Data.Packed.Vector
import Data.List

-- sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]


data Profile = Profile String (Matrix Float) SeqType Int deriving (Show)

blocks = "ACGT"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  Data.List.transpose $ map seqSequence seqs

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

convertMatrixIntsToFloats lists = map (map realToFrac) lists

getMatrix:: [MolSeq] -> Matrix Float
getMatrix seqs = fromLists $ Data.List.transpose $ convertMatrixIntsToFloats  $ map (letterCounts blocks) (seqsToStr seqs)

fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs seqs = do
	let typ = seqType (head seqs) -- Get type of first MolSeq
	let matrix = getMatrix seqs
	Profile "Matrix" matrix typ (length seqs)

--getWeightedMatrix :: Profile -> Matrix Float
--getWeightedMatrix (Profile _ matrix _ len) = mapMatrix (`div` realToFrac(len)) matrix

absSum matrix = sum $ map abs $ toList (flatten matrix)

--printRow :: Matrix -> Int -> String
--printRow matrix rowIndex = show $ Data.Vector.toList $ Data.Matrix.getRow rowIndex matrix

--instance Show Profile where
--  show (Profile name matrix typ len) = 
--  	(show name) ++ "\n" ++
--  	"A" ++ printRow matrix 1 ++ "\n" ++
--  	"C" ++ printRow matrix 2++ "\n" ++
--  	"G" ++ printRow matrix 3 ++ "\n" ++
--  	"T" ++ printRow matrix 4++ "\n" ++
--  	(show typ) ++ "\n" ++
--  	(show len)
