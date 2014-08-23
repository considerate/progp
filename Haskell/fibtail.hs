ackfib :: [Integer] -> [Integer]
ackfib [x,y] = [x] ++ ackfib [y,x+y]

fib :: Int -> Integer
fib n = last $ take n $ ackfib [0,1]

 
printFibList = mapM_ print (ackfib [0,1])

