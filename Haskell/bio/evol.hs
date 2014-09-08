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

distancePair :: (Evol a) => (a,a) -> (String, String, Double)
distancePair (x,y) = (name x, name y, distance x y)

crossSets :: (Eq a) => [a] -> [(a,a)]
crossSets xs = [(x,y) | x <- xs, y <- xs, (x `elemIndex` xs) <= (y `elemIndex` xs)]

distanceMatrix :: (Evol a) => [a] -> [(String, String, Double)]
distanceMatrix xs = map distancePair $ crossSets xs
