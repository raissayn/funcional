-- Define a função que separa [(Int, Char)] em ([Int], [Char])
myUnzip :: [(Int, Char)] -> ([Int], [Char])
myUnzip [] = ([], [])
myUnzip ((n, c):xs) = --n = int, c = char
    let (ns, cs) = myUnzip xs
    in (n:ns, c:cs)
-------
-- fib 
-------
--função f1
f1 :: String -> String -- recebe r e retorna s
f1 [] = [] -- caso base
f1 [A] = [A]
f1 (a:b:x) 
    | isDigit a && not(isDigit b) = a:(replica(num a)b) ++ f1 x
    | isDigit a && isDigit b      = a:f1(b:x)
    | otherwise                   = a:f1(b:x)
-- melhorar implementação
