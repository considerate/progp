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

-- sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]

data Profile = Profile String [[Double]] SeqType Int deriving (Show)

blocks = "ACGT"
proteinblocks = sort "ARNDCEQGHILKMFPSTWYVX"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  Data.List.transpose $ map seqSequence seqs

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

convertMatrixIntsToFloats lists = map (map realToFrac) lists

makeProfileMatrix :: [MolSeq] -> [[Double]]
makeProfileMatrix seqs = 
	case seqType $ head seqs of 
		DNA -> Data.List.transpose $ convertMatrixIntsToFloats $ map (letterCounts blocks) (seqsToStr seqs)
		Protein -> Data.List.transpose $ convertMatrixIntsToFloats $ map (letterCounts proteinblocks) (seqsToStr seqs)


molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name seqs = do
	let typ = seqType (head seqs) -- Get type of first MolSeq
	let matrix = makeProfileMatrix  seqs
	Profile name matrix typ (length seqs)

profileDiff :: Profile -> Profile -> [[Double]]
profileDiff (Profile _ a _ lena) (Profile _ b _ lenb) = matrixSub (matrixScale a (1/ realToFrac lena)) (matrixScale b (1/ realToFrac lenb))

profileDistance :: Profile -> Profile -> Double
profileDistance a b = sumMatrix $ matrixMap abs (profileDiff a b)

profileName :: Profile -> String
profileName (Profile name _ _ _) = name

profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile _ ma DNA lena) i c = do 
	let j = fromJust $ elemIndex c blocks
	(ma !! j !! i ) / realToFrac(lena)
profileFrequency (Profile _ ma Protein lena) i c = do 
	let j = fromJust $ elemIndex c proteinblocks
	(ma !! j !! i ) / realToFrac(lena)
