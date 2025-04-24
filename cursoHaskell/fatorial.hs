-- FATORIAL : 3! = 3*2*2 = 6.
-- função fatorial, recebe parametro n
{- fatorial(n) = 1 se n = 0
fatorial(n) = fatorial(n-1)*n se n>=1

main = print (soma 4)  -- Teste com n = 4 -}

fatorial 0 = 1
fatorial n = fatorial(n-1)*n


main = print (fatorial 4) 

{- ou por guards
fatorial n 
  | n == 0    = 1
  | otherwise = n * fatorial (n - 1)
-}