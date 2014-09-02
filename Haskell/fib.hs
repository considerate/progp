import System.Environment 

ackfib :: [Integer] -> [Integer]
ackfib [x,y] = [x] ++ ackfib [y,x+y]

fib :: Int -> Integer
fib n = last $ take n $ ackfib [0,1]


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