import Data.Char

{-01 função que retorna lista de duplas com char e posição na ASCII -}
listaDuplaCharInt :: Int -> [(Char, Int)]
listaDuplaCharInt n = [(chr x, x) | x <- [0..n]]

{-02 função meuChar que pesquisa um char pelo int na lista gerada -}
meuChar :: Int -> Int -> Maybe Char
meuChar limite x = lookup x [(i, chr i) | i <- [0..limite]]

{-03 função meuOrd que pesquisa o int pelo char na lista gerada -}
meuOrd :: Int -> Char -> Maybe Int
meuOrd limite c = lookup c [(chr i, i) | i <- [0..limite]]

{-04 função que ordena uma lista de inteiros -}
ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)
  where
    insere x [] = [x]
    insere x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insere x ys


{-05 seja o tipo [(Bool, [Int])]. 
Faça uma função que ordena [Int] quando o booleano é True. 
Também, passe o Bool para False, quando ordenar [Int]
exemplo: ordenaListaDupla [(True,[3,4,1,0,9]),(False,[]),(True,[4,3,2,1,0])]
retorna:                  [(False,[0,1,3,4,9]),(False,[]),(False,[0,1,2,3,4])]
-}
ordenaListaDupla :: [(Bool, [Int])] -> [(Bool, [Int])]
ordenaListaDupla [] = []
ordenaListaDupla ((True, xs):resto) = (False, ordenaLista xs) : ordenaListaDupla resto
ordenaListaDupla ((False, xs):resto) = (False, xs) : ordenaListaDupla resto
