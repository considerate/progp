module Levenshtein (
  levenshtein
)
where

-- http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Haskell

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

for_ xs f =  mapM_ f xs
levenshtein :: [Char] -> [Char] -> Int
levenshtein s t = d ! (ls , lt)
    where s' = array (0,ls) [ (i,x) | (i,x) <- zip [0..] s ]::UArray Int Char
          t' = array (0,lt) [ (i,x) | (i,x) <- zip [0..] t ]::UArray Int Char
          ls = length s
          lt = length t
          (l,h) = ((0,0),(length s,length t))
          d = runSTUArray $ do
                m <- newArray (l,h) 0 :: ST s (STUArray s (Int,Int) Int)
                for_ [0..ls] $ \i -> writeArray m (i,0) i
                for_ [0..lt] $ \j -> writeArray m (0,j) j
                for_ [1..lt] $ \j -> do
                              for_ [1..ls] $ \i -> do
                                  let c = if s'!(i-1)==t'! (j-1) 
                                          then 0 else 1
                                  x <- readArray m (i-1,j)
                                  y <- readArray m (i,j-1)
                                  z <- readArray m (i-1,j-1)
                                  writeArray m (i,j) $ minimum [x+1, y+1, z+c ]
                return m
