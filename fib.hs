import System.Environment 

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n-1) + fib (n-2)
      | otherwise = error "Negative indices are invalid"


listOfFib :: Int -> Int -> [Integer]
listOfFib x y = [fib z | z <- [x..y]]

parseInt :: String -> Int
parseInt x = read x

result [x] = Left (fib (parseInt x))
result [x, y] = Right (listOfFib (parseInt x) (parseInt y))

main :: IO ()
main = do 
	args <- getArgs
	let value = result args
	putStrLn $ either show show value -- Convert to string in either case