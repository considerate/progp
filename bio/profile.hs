module Profile
(Profile,
profileDistance,
profileDiff,
fromMolSeqs
)
where

import MolSeq

import qualified Numeric.Matrix as M
import Data.List

-- sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]

data Profile = Profile String (M.Matrix Double) SeqType Int deriving (Show)

blocks = "ACGT"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  Data.List.transpose $ map seqSequence seqs

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

convertMatrixIntsToFloats lists = map (map realToFrac) lists

makeProfileMatrix :: [MolSeq] -> M.Matrix Double
makeProfileMatrix seqs = M.fromList $ Data.List.transpose $ convertMatrixIntsToFloats $ map (letterCounts blocks) (seqsToStr seqs)


fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs seqs = do
	let typ = seqType (head seqs) -- Get type of first MolSeq
	let matrix = makeProfileMatrix  seqs
	Profile "Matrix" matrix typ (length seqs)

profileDiff :: Profile -> Profile -> M.Matrix Double
profileDiff (Profile _ a _ lena) (Profile _ b _ lenb) = M.minus (M.scale a (1/ realToFrac lena)) (M.scale b (1/ realToFrac lenb))

sumMatrix :: (Num a) => [[a]] -> a
sumMatrix xs = sum $ map sum xs

profileDistance :: Profile -> Profile -> Double
profileDistance a b = sumMatrix $ M.toList $ M.map abs (profileDiff a b)
