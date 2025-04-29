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
invert1 [] = []
invert1 (x:xs) = invert x : invert1 xs

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
myHead :: [Int] -> Int
myHead [] = []
myHead (x:_) = x

{- myTail que recebe uma lista x e retorna a lista x sem a cabeça -}
myTail :: [Int] -> [Int]
myTail [] = []
myTail (_:xs) = xs

{- myLast que recebe uma lista x e retorna o último elemento de x -}
myLast :: [Int] -> Int
myLast [] = []
myLat (_:xs) = myLast xs

{- myInit que recebe uma lista x e retorna a lista x sem o último elemento -}
myInit :: [Int] -> [Int] 
myInit [] = []
myInit [x] = []
myInit (x:xs) = x : myInit xs 

{- Exercício: Implementar a função que retorna o maior elemento de uma lista -}
maiorElemento :: [Int] -> Int
maiorElemento [] = error "Lista vazia"
maiorElemento [x] = x
maiorElemento (x:xs) = max x (maiorElemento xs)

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
totalVendas :: Int -> Int
totalVendas d = f d + totalVendas (d - 1) --lembrar d-1

{-encontra o dia em que mais se vendeu no período - versão 01-}
DiaMaiorVenda01 :: Int -> Int
DiaMaiorVenda01 y  
    | f y >= f (diaMaiorVenda01(y-1)) = y
    | otherwise = diaMaiorVenda01(y-1)

{-encontra o dia em que mais se vendeu no período - versão 02
  tem como parâmetros o período e a maior venda -}
diaMaiorVenda02 Int -> Int -> Int

{- encontra o maior valor entre dois inteiros -}
maxi Int -> Int -> Int
maxi a b
  | a > b = a
  | otherwise = b

{- encontra a maior venda - 
   Exercício: implemente essa função com apenas dois parâmetros e 
   fazendo uso de maxi no código interno-}
maiorVenda Int -> Int -> Int
maiorVenda x y
  | 
{- media vendas-}

-----------------------------------------------------------------------------------
{-SCRIPT O3 -}
{- Assunto: listas e tuplas -}
periodo::Int
periodo = 7

-- implementar função maxi 

-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

{- 01 função que retorna uma lista de vendas -}


{- 02 função que retorna [[Int]] com listas de dia e venda -}

  
----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
--ordenaLista::[Int]->[Int]

-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
--ordenaListaLista::[[Int]]->[[Int]]

---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
{- 07 função que gera o total de vendas-}
--totalVendasT::[(Int, Int)] -> Int


{- 08 função que retorna a maior venda -}
  
{- 08-b como implementar com apenas os parâmetros? 
  maiorVendaT::[(Int, Int)] -> Int  -}  
  
{- 09 função que retorna os dias das maiores vendas -}
