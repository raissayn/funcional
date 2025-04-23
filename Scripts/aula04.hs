{- objetivo: trabalhar tipos distintos entre listas e tuplas -}

import Data.Char
{- Função pega tupa e transforma em lista -}
f2 :: [(Int,Char)] -> ([Int], [Char])
f2 [] = ([],[])
f2 ((a,b):x) = (a : fst (f2 x), b : snd (f2 x))

{- o mesmo que a f2 porém utilizando funções auxiliares-}
f3 :: [(Int,Char)]->([Int],[Char])
f3 x = (geraLInt x , geraLChar x)

geraLInt :: [(Int,Char)]->[Int]
geraLInt [] = []
geraLInt (a:x) = fst a:geraLInt x

geraLChar:: [(Int,Char)]->[Char]
geraLChar [] = []
geraLChar (a:x) = snd a:geraLChar x
------------------------------------------------------
{- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}
myUnzip :: [(Int, Char)] -> ([Int], [Char])
myUnzip [] = ([], [])
myUnzip ((i,c):xs) =
  let (is, cs) = myUnzip xs
  in (i:is, c:cs)

{- 02 versão em uma única função -}
myUnzipU :: [(Int, Char)] -> ([Int], [Char])
myUnzipU = foldr (\(i,c) (is,cs) -> (i:is, c:cs)) ([],[])

------------------------------------------------------------
{- 03 função que junta duas listas em lista de duplas -}
myZip :: [Bool] -> [Char] -> [(Bool, Char)]
myZip [] _ = []
myZip _ [] = []
myZip (b:bs) (c:cs) = (b,c) : myZip bs cs

{- 04 função que recebe [Char] e retorna [(Bool,Char)] 
   True se Char for alfanumérico e False, caso contrário -}
setAlfa :: String -> [(Bool, Char)]
setAlfa = map (\c -> (isAlphaNum c, c))

{- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
filtraAlfa :: [(Bool, Char)] -> String
filtraAlfa = map snd . filter fst

{- 06 função transforma String de alfa em Int -}
alfaToInt :: String -> [Int]
alfaToInt = map digitToInt . filter isDigit

{-- 07 função que gera tabela ascii -}
f4 (-1) = []
f4 x = (x, chr x) : f4 (x-1)
