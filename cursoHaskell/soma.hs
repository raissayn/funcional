{-
N = 4 ⇒ 1 + 2 + 3 + 4 = 10

N = 3 ⇒ 1 + 2 + 3 = 6

soma(n)
se o n=1, então soma(n)=1
se o n>1, então soma(n)=soma(n-1)+1 
-}

-- main = putStrLn "Olá, Haskell no VS Code!"
soma 1 = 1 -- caso base
soma n = soma(n-1) + n -- caso recursivo

main = print (soma 4)  -- Teste com n = 4
