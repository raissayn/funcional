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
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs 

{- 02-localiza elemento em lista -}
localiza :: Int -> [Int] -> Bool
localiza _ [] = False
localiza y (x:xs)
  | x == y = True
  | otherwise = localiza y xs

{-03 remove todas ocorrências de y em uma lista -}
removOcorrencia :: Int -> [Int] -> [Int]
removOcorrencia _ [] = []
removOcorrencia y (x:xs)
  | y == x = removOcorrencia y xs
  | otherwise = x : removOcorrencia y xs

{-04 informa o tamanho de uma lista -}
tamLista [Int] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

{-05 conta a ocorrência de um Int em [Int] -}
ocorrencia Int -> [Int] -> Int
ocorrencia _ [] = 0
ocorrencia y (x:xs)
  | x == y = 1 + ocorrencia y xs
  | otherwise = ocorrencia y xs

{- 06 inverte a lista -}
invert [Int] -> [Int]
invert [] = []
invert (x:xs) = invert xs ++ [x]

{- 07 inverte elementos das listas internas -}
invert1 [[Int]] -> [[Int]]
invert [] = []
invert (x:xs) = 
{- 08 função que exclui a penúltima ocorrência de um número na lista-}
-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}    

{-myHead que recebe uma lista x e retorna a cabeça de x-}

{- myTail que recebe uma lista x e retorna a lista x sem a cabeça -}

{- myLast que recebe uma lista x e retorna o último elemento de x -}

{- myInit que recebe uma lista x e retorna a lista x sem o último elemento -}

{- Exercício: Implementar a função que retorna o maior elemento de uma lista -}

-----------------------------------------------------------------------------------
{-SCRIPT O2 -}
ype Dia = Int
type VendaR = Int

-- define o período de recursão
periodo::Int
periodo = 7

-- tabela de vendas
f :: Int -> Int
f 1 = 41
f 2 = 72
f 3 = 48   
f 4 = 2
f 5 = 91
f 6 = 55
f 7 = 30
f _ = 0

{- retorna o total de vendas do período -}
{-encontra o dia em que mais se vendeu no período - versão 01-}

{-encontra o dia em que mais se vendeu no período - versão 02
  tem como parâmetros o período e a maior venda -}

{- encontra o maior valor entre dois inteiros -}

{- encontra a maior venda - 
   Exercício: implemente essa função com apenas dois parâmetros e 
   fazendo uso de maxi no código interno-}

{- media vendas}

-----------------------------------------------------------------------------------
{-SCRIPT O3 -}