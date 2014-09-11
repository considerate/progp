module Profile
(Profile,
profileDistance,
profileDiff,
profileFrequency,
profileName,
molseqs2profile
)
where

import MolSeq

import Data.List
import Data.Maybe
import Matrix

data Profile = Profile String [[Double]] [[Double]] SeqType Int deriving (Show, Eq)

blocks :: String
blocks = "ACGT"

proteinblocks :: String
proteinblocks = "ACDEFGHIKLMNPQRSTVWXY"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  transpose $ map seqSequence seqs

letterRow :: String -> String -> [Double]
letterRow letters str = map realToFrac (letterCounts letters str)

createMatrixForBlocks :: String -> [MolSeq] -> [[Double]]
createMatrixForBlocks letters seqs = transpose $ map (letterRow letters) (seqsToStr seqs)

makeProfileMatrix :: [MolSeq] -> [[Double]]
makeProfileMatrix seqs = 
	case seqType $ head seqs of 
		DNA -> createMatrixForBlocks blocks seqs
		Protein -> createMatrixForBlocks proteinblocks seqs

molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name seqs = do
	let typ = seqType (head seqs) -- Get type of first MolSeq
	let matrix = makeProfileMatrix seqs
	let len = length seqs
	let scaled = matrixScale matrix (1 / realToFrac len)
	Profile name matrix scaled typ len

absDiff :: (Num a) => a -> a -> a
absDiff x y = abs (x-y)

profileDiff :: Profile -> Profile -> [[Double]]
profileDiff (Profile _ _ a _ _) (Profile _ _ b _ _) = matrixApply absDiff a b

profileDistance :: Profile -> Profile -> Double
profileDistance a b = sumMatrix $ profileDiff a b

profileName :: Profile -> String
profileName (Profile name _ _ _ _) = name

profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile _ _ ma DNA _) i c = do 
	let j = fromJust $ elemIndex c blocks
	ma !! j !! i
profileFrequency (Profile _ _ ma Protein _) i c = do 
	let j = fromJust $ elemIndex c proteinblocks
	ma !! j !! i 
