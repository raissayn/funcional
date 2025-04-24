-- FIBONACCI
{-
se n = 0, então fib(n)=0
se n = 1, então fib(n)=1
se n > 1, então fib(n)=fib(n-1) + fib(n-2)

exemplo: n = 3
fib(3) = fib(2) + fib(1) = 1 + 1 = 2
fib(1) = 1
fib(2) = fib(1) + fib(0) = 1 + 0 = 1
fib(0) = 0
-}


-- ou guards
fib :: Int -> Int
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

main :: IO ()
main = print (fib 0)