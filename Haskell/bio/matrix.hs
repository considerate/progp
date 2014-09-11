module Matrix
(matrixApply,
matrixScale,
matrixMap,
matrixSub,
sumMatrix)
where


matrixApply :: (Num a) => (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
matrixApply f = zipWith (zipWith f)

sub :: (Num a) => a -> a -> a
sub x y = x - y

matrixSub :: (Num a) => [[a]] -> [[a]] -> [[a]]
matrixSub = matrixApply sub

sumMatrix :: (Num a) => [[a]] -> a
sumMatrix xs = sum $ map sum xs

matrixMap :: (Num a, Num b) => (a -> b) -> [[a]] -> [[b]]
matrixMap f = map (map f)

matrixScale :: (Num a) => [[a]] -> a -> [[a]]
matrixScale m x = matrixMap (*x) m