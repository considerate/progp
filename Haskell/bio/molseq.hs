module MolSeq
(	 
	MolSeq,
	SeqType(DNA,Protein),
	string2seq,
	seqType,
	seqName,
	seqDistance,
	seqSequence,
	seqLength
)
where

import Hamming
import Data.Function

data SeqType = DNA | Protein deriving (Show, Eq)
data MolSeq = MolSeq String String SeqType deriving (Show, Eq)

dnaBlocks :: String
dnaBlocks = "ACGT"

isProtein :: String -> Bool
isProtein s = not $ isDNA s

isDNA :: String -> Bool
isDNA = all (`elem` dnaBlocks)

string2seq :: String -> String -> MolSeq
string2seq name s | isDNA s = MolSeq name s DNA
				  | isProtein s = MolSeq name s Protein

seqName :: MolSeq -> String
seqName (MolSeq name _ _) = name

seqSequence :: MolSeq -> String
seqSequence (MolSeq _ s _) = s

seqLength :: MolSeq -> Int
seqLength (MolSeq _ s _) = length s

seqType :: MolSeq -> SeqType
seqType (MolSeq _ _ typ) = typ 

{-
 Enligt en känd och enkel modell som kallas Jukes-Cantor låter man avståndet da, b mellan två DNA-sekvenser
 a och b vara da,b= -3/4ln(1-4α/3) där α är andelen positioner där sekvenserna skiljer sig åt. 

 Formeln fungerar inte bra om sekvenserna skiljer sig åt mer än väntat, så om α>0.74 låter man ofta da, b = 3.3.
-}

divide :: Int -> Int -> Double
divide = (/) `on` realToFrac

seqDist :: String -> String -> Double
seqDist a b = divide (hammingDist a b) (length a)

seqDistance :: MolSeq -> MolSeq -> Double
seqDistance (MolSeq _ a DNA) (MolSeq _ b DNA) | seqDist a b <= 0.74 = -3 / 4 * log(1 - 4/3 * seqDist a b)
											  | otherwise = 3.3
seqDistance (MolSeq _ a Protein) (MolSeq _ b Protein) | seqDist a b <= 0.94 =  -19 / 20 * log(1 - 20/19 * seqDist a b)
                                                      | otherwise = 3.7
seqDistance _ _ = error "Incompatible Sequence types"