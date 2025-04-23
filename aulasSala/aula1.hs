-- script aula02 - 8 função que exclui a penultima ocorrência de um número na lista
contList:: Int ->[Int]->[Int]
contList _ [] = 0
contList x (a:b)
    | x==a = 1 + contList*b
    | otherwise = 0 + contList*b
Associativa à direita

exP:: Int->Inr->[Int]->[Int] 
exP _ _ [] = []
exP _ _ (a:[]) = [a]
exP r c (a:x)
    | c == 2 && r == a = x
    |           r == a = a:exP r (c-1) x
    | otherwise        = a:exP r c x

-------------------------------------------------------------------------------

-- outra forma: percorrendo toda a lista 
