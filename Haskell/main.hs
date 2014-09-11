module Main where

import F1
import Control.DeepSeq

main = do 
	let a = karpsravor $ rovarsprak (take 1000000 $ repeat 'n')
	b <- readFile "kipling.txt"
	let c = take 10000 (repeat b)
	let d = map medellangd c
	d `deepseq` a `deepseq` print "Done"
