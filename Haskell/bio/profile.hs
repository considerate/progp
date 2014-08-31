module Profile
(Profile,
profileDistance,
profileDiff,
fromMolSeqs
)
where

import MolSeq

import Data.List

-- sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]

data Profile = Profile String [[Double]] SeqType Int deriving (Show)

blocks = "ACGT"

countLetter :: String -> Char -> Int
countLetter str x = length $ filter (== x) str

seqsToStr :: [MolSeq] -> [String]
seqsToStr seqs =  Data.List.transpose $ map seqSequence seqs

letterCounts :: String -> String -> [Int]
letterCounts letters str = map (countLetter str) letters

convertMatrixIntsToFloats lists = map (map realToFrac) lists

makeProfileMatrix :: [MolSeq] -> [[Double]]
makeProfileMatrix seqs = Data.List.transpose $ convertMatrixIntsToFloats $ map (letterCounts blocks) (seqsToStr seqs)


fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs seqs = do
	let typ = seqType (head seqs) -- Get type of first MolSeq
	let matrix = makeProfileMatrix  seqs
	Profile "Matrix" matrix typ (length seqs)

applyPair f (x,y) = f x y
applyLists f (x,y) = map (applyPair f) $ zip x y

matrixApply :: (Num a) => (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
matrixApply f ma mb = map (applyLists f) $ zip ma mb

sub :: (Num a) => a -> a -> a
sub x y = x - y

matrixSub :: (Num a) => [[a]] -> [[a]] -> [[a]]
matrixSub ma mb = matrixApply sub ma mb

profileDiff :: Profile -> Profile -> [[Double]]
profileDiff (Profile _ a _ lena) (Profile _ b _ lenb) = matrixSub (matrixScale a (1/ realToFrac lena)) (matrixScale b (1/ realToFrac lenb))

sumMatrix :: (Num a) => [[a]] -> a
sumMatrix xs = sum $ map sum xs

matrixMap :: (Num a, Num b) => (a -> b) -> [[a]] -> [[b]]
matrixMap f ma = map (map f) ma 

matrixScale :: (Num a) => [[a]] -> a -> [[a]]
matrixScale m x = matrixMap (*x) m

profileDistance :: Profile -> Profile -> Double
profileDistance a b = sumMatrix $ matrixMap abs (profileDiff a b)
