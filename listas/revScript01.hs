-- treino refazendo os códigos feitos em sala de aula
{- SCRIPT 01 -}
{-Assunto: Litas
  Os conceitos introdutórios sobre listas foram apresentados em sala.
  Agora, considerando os casos mais simples, com apenas listas de inteiros,
  implemente as funções abaixo, considerando os operadores ++ e :
    ++ (concatena listas)
     : (insere um elemento na lista)
-}

     
{- 01 função que soma os elementos de uma lista -}
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs 

{- 02-localiza elemento em lista -}
localiza :: Int -> [Int] -> Bool
localiza _ [] = False
localiza y (x:xs)
  | y == x = True
  | otherwise = localiza y xs

{-03 remove todas ocorrências de y em uma lista -}
removeOcorrencia :: Int -> [Int] -> [Int]
removeOcorrencia _ [] = []
removeOcorrencia y (x:xs)
  | y == x = removeOcorrencia y xs
  | otherwise = x : removeOcorrencia y xs 

{-04 informa o tamanho de uma lista -}
tamLista :: [Int] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

{-05 conta a ocorrência de um Int em [Int] -}
ocorrencia :: Int -> [Int] -> Int
ocorrencia _ [] = 0
ocorrencia y (x:xs)
  | y == x = 1 + ocorrencia y xs
  | otherwise = ocorrencia y xs

{- 06 inverte a lista -}
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

{- 07 inverte elementos das listas internas -}
inverteListInter :: [[Int]] -> [[Int]]
inverteListInter [] = []
inverteListaInter (x:xs) = inverteLista x : inverteListaInter xs

-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}    

{-myHead que recebe uma lista x e retorna a cabeça de x-}
myHead :: [Int] -> Int
myHead [] = error "lista vazia"
myHead (x:_) = x 

{- myTail que recebe uma lista x e retorna a lista x sem a cabeça -}
myTail :: [Int] -> [Int]
myTail [] = error "lista vazia"
myTail (x:xs) = xs

{- myLast que recebe uma lista x e retorna o último elemento de x -}-
myLast [Int] -> Int
myLast [] = error "lista vazia"
myLast [x] = x
myLast (__:xs) = myLast xs

{- myInit que recebe uma lista x e retorna a lista x sem o último elemento -}
myInit :: [Int] -> [Int]
myInit [] = error "lista vazia"
myInit [a] = []
myInit (x:xs) = x : myInit xs

{- Exercício: Implementar a função que retorna o maior elemento de uma lista -}
maiorLista :: [Int] -> Int
maiorLista [] = error "lista vazia"
maiorLista [x] = x
maiorLista (x:xs) = max x (maiorLista xs)