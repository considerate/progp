module Evol
(
	Evol,
	distanceMatrix
)
where

import MolSeq
import Profile
import Matrix

class Evol a where
	distance :: a -> a -> Double
	name :: a -> String

instance Evol MolSeq where
	distance x y = seqDistance x y
	name x = seqName x

instance Evol Profile where
	distance x y = profileDistance x y
	name x = profileName x



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


distanceMatrix :: (Evol a) => [a] -> PrettyMatrix Double a
distanceMatrix xs = PrettyMatrix (distanceMatrix' xs) xs