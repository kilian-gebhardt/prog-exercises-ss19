fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

sumFacs :: Int -> Int -> Int
sumFacs n m = if n > m then 0
              else fac n + sumFacs (n+1) m

sumFacs' :: Int -> Int -> Int
sumFacs' n m | n > m     = 0
             | otherwise = fac n + sumFacs' (n+1) m

fib ::  Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
