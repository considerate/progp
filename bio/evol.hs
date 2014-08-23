module Evol
(distanceMatrix)
where

import MolSeq
import Profile
import qualified Numeric.Matrix as M

class Evol a where
	distance :: a -> a -> Double

instance Evol MolSeq where
	distance x y = seqDistance x y

instance Evol Profile where
	distance x y = profileDistance x y


distanceRow :: (Evol a) => [a] -> a -> [Double]
distanceRow xs y = map (\z -> distance y z) xs

distanceMatrix :: (Evol a) => [a] -> M.Matrix Double
distanceMatrix xs = M.fromList $ map (distanceRow xs) xs 
