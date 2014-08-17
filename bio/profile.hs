module Profile
(Profile)
where

import MolSeq

import Data.Matrix
import Data.Map

sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]


data Profile = Profile String Matrix SeqType Integer deriving(Show)


fromMolSeqs :: [MolSeq] -> [String]
fromMolSeqs seqs = map head $ map seqSequence seqs