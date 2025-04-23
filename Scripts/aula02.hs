{-Assunto: Litas
  Os conceitos introdutórios sobre listas foram apresentados em sala.
  Agora, considerando os casos mais simples, com apenas listas de inteiros,
  implemente as funções abaixo, considerando os operadores ++ e :
    ++ (concatena listas)
     : (insere um elemento na lista)
-}
     
{- 01 função que soma os elementos de uma lista -}
sumList::[Int]->Int
sumList [] = 0
sumList (x:xs) = x + sumList xs    


{- 02-localiza elemento em lista -}
searchList::Int->[Int]->Bool
searchList _ [] = False
searchList y (x:xs)
  | y == x    = True                      -- Se o elemento for igual ao primeiro, achou!
  | otherwise = searchList y xs   


{-03 remove todas ocorrências de y em uma lista -}
deleteList::Int->[Int]->[Int]
deleteList _ _ = []
deleteList y (x:xs)
  | y == x    = deleteList y xs           -- Se for igual, ignora o elemento
  | otherwise = x : deleteList y xs  

{-04 informa o tamanho de uma lista -}
lenghtList::[Int]->Int
lengthList [] = 0                         -- Lista vazia tem tamanho 0
lengthList (_:xs) = 1 + lengthList xs  -- Conta 1 e continua no resto da lista

{-05 conta a ocorrência de um Int em [Int] -}
contList :: Int -> [Int] -> Int
contList _ [] = 0                           -- Lista vazia tem 0 ocorrências
contList y (x:xs)
  | y == x    = 1 + contList y xs           -- toda vez que encontrar y, soma 1
  | otherwise = contList y xs               -- Senão, continua procurando

{- 06 inverte a lista -}
reverseList :: [Int] -> [Int]
reverseList [] = []                         -- Lista vazia invertida é vazia
reverseList (x:xs) = reverseList xs ++ [x]  -- Coloca o primeiro elemento no final da lista invertida

{- 07 inverte elementos das listas internas -}
reverseInnerLists :: [[Int]] -> [[Int]]
reverseInnerLists [] = []
reverseInnerLists (x:xs) = reverseList x : reverseInnerLists xs

{- 08 função que exclui a penúltima ocorrência de um número na lista-}
-- Conta ocorrências (reutilizando a função 05)
ocorrencia :: [Int] -> Int -> Int
ocorrencia = contList

-- Remove a penúltima ocorrência
app :: [Int] -> Int -> Int -> [Int]
app [] _ _ = []
app (a:b) x i
  | a == x && i == 2 = b                     -- Remove penúltima ocorrência
  | a == x           = a : app b x (i - 1)   -- Diminui o contador
  | otherwise        = a : app b x i         -- Mantém o mesmo contador

-- Interface principal
appS :: [Int] -> Int -> [Int]
appS l x = app l x (ocorrencia l x)
-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}       
myHead :: [Int] -> Int
myHead [] =  error "Lista vazia!" 
myHead (x:_) = x

myTail :: [Int] -> [Int]
myTail [] =  error "Lista vazia!" 
myTail (_:xs) = xs

myLast :: [Int] -> Int
myLast [] =  error "Lista vazia!" 
myLast [a] = a
myLast (x:xs) = myLast xs

myInit :: [Int] -> [Int]
myInit [] =  error "Lista vazia!" 
myInit [a] = []
myInit (x:xs) = x : myInit xs
------
tamanhoL :: [u] -> Int
tamanhoL [] = 0
tamanhoL (_:b) = 1 + tamanhoL b
------
apaga :: [Int] -> Int -> [Int]
apaga [] _ = []
apaga (a:x) y 
  | a == y    = apaga x y         -- CORRETO: remove e continua
  | otherwise = a : (apaga x y)
------
ocorrencia :: [Int] -> Int -> Int 
ocorrencia [] _ = 0
ocorrencia (a:b) x 
  | a == x    = 1 + (ocorrencia b x)
  | otherwise = ocorrencia b x
------
app :: [Int] -> Int -> Int -> [Int]
app [] _ _ = [] 
app (a:b) x i
  | a == x && i == 2 = b
  | a == x           = a : app b x (i - 1)
  | otherwise        = a : app b x i

appS :: [Int] -> Int -> [Int]
appS l x = app l x (ocorrencia l x)
------
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]
------
inverteTodas :: [[Int]] -> [[Int]]
inverteTodas [] = []
inverteTodas (a:x) = inverteLista a : inverteTodas x
-- ou  [(inverteLista a)] ++ inverteTodas x

