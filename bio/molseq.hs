module MolSeq
(	 
	MolSeq,
	string2seq,
	seqDistance
)
where

import Levenshtein

data SeqType = DNA | Protein deriving (Show)
data MolSeq = MolSeq String String SeqType deriving (Show)

dnaBlocks = ['A', 'C', 'G', 'T']

isProtein :: String -> Bool
isProtein seq = not $ isDNA seq

isDNA :: String -> Bool
isDNA seq = all (`elem` dnaBlocks) seq

string2seq :: String -> String -> MolSeq
string2seq name seq | isDNA seq = (MolSeq name seq DNA)
					| isProtein seq = (MolSeq name seq Protein)

seqName :: MolSeq -> String
seqName (MolSeq name _ _) = name

seqSequence :: MolSeq -> String
seqSequence (MolSeq _ seq _) = seq

seqLength :: MolSeq -> Int
seqLength (MolSeq _ seq _) = length seq

seqType :: MolSeq -> SeqType
seqType (MolSeq _ _ type) = type 

{-
 Enligt en känd och enkel modell som kallas Jukes-Cantor låter man avståndet da, b mellan två DNA-sekvenser
 a och b vara da,b= -3/4ln(1-4α/3) där α är andelen positioner där sekvenserna skiljer sig åt. 

 Formeln fungerar inte bra om sekvenserna skiljer sig åt mer än väntat, så om α>0.74 låter man ofta da, b = 3.3.
-}

fractionalDist :: Int -> Int -> Int -> Float
fractionalDist dist 0 0 = 0
fractionalDist dist lena lenb | lena > lenb = realToFrac(dist) / realToFrac(lena)
							  | otherwise = realToFrac(dist) / realToFrac(lenb)

seqDist :: String -> String -> Float
seqDist a b = fractionalDist (levenshtein a b) (length a) (length b)

seqDistance :: MolSeq -> MolSeq -> Float
seqDistance (MolSeq _ a DNA) (MolSeq _ b DNA) = 
	-3 / 4 * log(1 - 4/3 * seqDist a b)

seqDistance (MolSeq _ a Protein) (MolSeq _ b Protein) = 
	-19 / 20 * log(1 - 20/19 * seqDist a b)