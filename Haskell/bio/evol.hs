module Evol
(
	Evol(name,distance),
	distanceMatrix
)
where

import MolSeq
import Profile
import Matrix
import Data.List

class (Eq a) => Evol a where
	distance :: a -> a -> Double
	name :: a -> String

instance Evol MolSeq where
	distance = seqDistance
	name = seqName

instance Evol Profile where
	distance = profileDistance
	name = profileName

data PrettyMatrix a b = PrettyMatrix [[a]] [b] deriving (Eq)

instance (Show a, Evol b) => Show (PrettyMatrix a b) where
	show (PrettyMatrix ma evols) = 
		(showHeader $ map name evols) ++ "\n" ++ 
		(unlines . map (\ (row, e) -> showRow (name e) row) $ zip ma evols)
		where
			showRow n row = n ++ (unwords $ map (("\t\t"++) . show) row)
			showHeader row = (unwords $ map (("\t\t"++)) row)

roundToDec n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

distanceRow :: (Evol a) => [a] -> a -> [Double]
distanceRow xs y = map (\z -> distance y z) xs

distanceMatrix' :: (Evol a) => [a] -> [[Double]]
distanceMatrix' xs = matrixMap (roundToDec 2) (map (distanceRow xs) xs)

distancePair :: (Evol a) => (a,a) -> (String, String, Double)
distancePair (x,y) = (name x, name y, distance x y)

crossSets :: (Eq a) => [a] -> [(a,a)]
crossSets xs = [(x,y) | x <- xs, y <- xs, (x `elemIndex` xs) <= (y `elemIndex` xs)]

distanceMatrix :: (Evol a) => [a] -> [(String, String, Double)]
distanceMatrix xs = map distancePair $ crossSets xs
