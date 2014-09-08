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

data Profile = Profile String [[Double]] [[Double]] SeqType Int deriving (Show, Eq)

blocks = "ACGT"
proteinblocks = sort "ARNDCEQGHILKMFPSTWYVX"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  Data.List.transpose $ map seqSequence seqs

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

makeProfileMatrix :: [MolSeq] -> [[Double]]
makeProfileMatrix seqs = 
	case seqType $ head seqs of 
		DNA -> Data.List.transpose $ matrixMap realToFrac $ map (letterCounts blocks) (seqsToStr seqs)
		Protein -> Data.List.transpose $ matrixMap realToFrac $ map (letterCounts proteinblocks) (seqsToStr seqs)

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
profileDiff (Profile _ _ a _ lena) (Profile _ _ b _ lenb) = matrixApply absDiff a b

profileDistance :: Profile -> Profile -> Double
profileDistance a b = sumMatrix $ profileDiff a b

profileName :: Profile -> String
profileName (Profile name _ _ _ _) = name

profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile _ _ ma DNA lena) i c = do 
	let j = fromJust $ elemIndex c blocks
	(ma !! j !! i )
profileFrequency (Profile _ _ ma Protein lena) i c = do 
	let j = fromJust $ elemIndex c proteinblocks
	(ma !! j !! i )
