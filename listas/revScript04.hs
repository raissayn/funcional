{- objetivo: trabalhar tipos distintos entre listas e tuplas -}

import Data.Char


-- ------------------------------------------------------
-- {- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}



-- {- 02 versão em uma única função -}
-- --myUnzipU :: [(Int,Char)]->([Int],[Char])

-- ------------------------------------------------------------
-- {- 03 função que junta duas listas em lista de duplas -}
-- --myZip::[Bool]->[Char] ->[(Bool,Char)]  

myZip :: [Bool] -> [Char] -> [(Bool,Char)]
myZip [] _ = []
myZip _ [] = []
myZip (b:bs)(c:cs) = (b,c): myZip bs cs


-- {- 04 função que recebe [Char] e retorna [(Bool,Char)] 
--    True se Char for alfanumérico e False, caso contrário -}
-- --setAlfa::String -> [(Bool,Char)] 


   
-- {- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
-- --filtraAlfa:: [(Bool,Char)] -> String


-- {- 06 função transforma String de alfa em Int -}
-- --alfaToInt::String -> [Int]

-- {-O map é usado para aplicar a função ord (que converte um Char em Int)
-- a cada caractere da String fornecida, gerando a lista de códigos ASCII 
-- correspondente. Se quiser entender melhor como o map funciona, ele
--  aplica uma função em todos os elementos de uma lista e retorna uma nova lista com os resultados. Neste caso, a função é ord.-}

-- {-- 07 função que gera tabela ascii -}
-- --geraASCII::Int->[(Int,Char)]


