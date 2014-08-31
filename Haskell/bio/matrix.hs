module Matrix
(matrixApply,
matrixScale,
matrixMap,
matrixSub,
sumMatrix)
where


applyPair f (x,y) = f x y
applyLists f (x,y) = map (applyPair f) $ zip x y

matrixApply :: (Num a) => (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
matrixApply f ma mb = map (applyLists f) $ zip ma mb

sub :: (Num a) => a -> a -> a
sub x y = x - y

matrixSub :: (Num a) => [[a]] -> [[a]] -> [[a]]
matrixSub ma mb = matrixApply sub ma mb

sumMatrix :: (Num a) => [[a]] -> a
sumMatrix xs = sum $ map sum xs

matrixMap :: (Num a, Num b) => (a -> b) -> [[a]] -> [[b]]
matrixMap f ma = map (map f) ma

matrixScale :: (Num a) => [[a]] -> a -> [[a]]
matrixScale m x = matrixMap (*x) m