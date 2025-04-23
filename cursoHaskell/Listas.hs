-- listas comprimento
size_list []=0
size_list (a:b) = 1 + size_list b -- retirando a cabeça da lista e contando o corpo/CHAMADA RECURSIVA
-------
-- função que verifica se duas listas são iguais( mesmo elementos e numeros de elementos na mesma posição)
comp_listas :: [Int] -> [Int] -> Bool
comp_listas [] [] = True
comp_listas [] _ = False
comp_listas _ [] = False
comp_listas (a:b) (c:d) 
    | (a == c) = comp_listas b d
    | otherwise = False
--------
-- função que retorna o inverso da lista
inv_listas :: [t] -> [t]
inv_listas [] = []
inv_listas (a:b) = inv_listas b ++ [a]
--------
-- pertence 
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence n (a:b)
    | n == a    = True
    | otherwise = pertence n b
--------
-- maior elemento da lista
maior :: [Int] -> Int
maior [x] = x
maior (a:b) 
    | (a > maior b) = a
    | otherwise = maior b
--------
-- verifica se os elementos da lista são pares
todos_pares :: [Int] -> Bool
todos_pares [] = True
todos_pares (a:resto) 
    | (mod a 2 == 1) = False
    | otherwise= todos_pares resto
--------
-- funçao zip, recebe duas listas e forma uma tupa com a combinação das listas
zip :: [a] -> [b] -> [(a,b)]
--------
-- ordenar listas crescentemente
lista :: [Int]
lista = [5,10,3,1,9]
get_menor :: [Int] -> Int
get_menor [x] = x
get_menor (x:xs)
    |(x < get_menor xs) = x
    | otherwise = get_menor xs

remove_menor :: [Int] -> [Int]
remove_menor [] = []
remove_menor (x:xs) 
    | ( x == (get_menor(x:xs)))
    | otherwise = (x:remove_menor xs)

aux_ordena :: [Int] -> [Int] -> [Int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = aux_ordena (lista_ordenada++[get_menor(x:xs)]) (remove_menor(x:xs))

ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = aux_ordena [] lista
---------
-- inverte lista
inverte :: [a] -> [a]
inverte [] -> []
inverte (x:xs) = (inverte xs) ++ [x]
---------
--