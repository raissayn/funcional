-- define o período de recursão
periodo :: Int
periodo = 7

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

-- função que retorna lista de vendas (do dia x até o 0)
--monta uma lista com as vendas dos dias de forma decrescente.
listaVendas :: Int -> [Int]
listaVendas (-1) = []
listaVendas x = vendas x : listaVendas (x-1) 

-- função que retorna lista de [dia, venda]
-- essa função devolve uma lista de listas, onde cada sublista tem dois elementos:
[dia, vendas no dia]
listaDiaVendas :: Int -> [[Int]]
listaDiaVendas (-1) = []
listaDiaVendas x = [x, vendas x] : listaDiaVendas (x-1)

-- versão automática usando 'periodo'
listaVendasPeriodo :: [Int]
listaVendasPeriodo = listaVendas periodo

listaDiaVendasPeriodo :: [[Int]]
listaDiaVendasPeriodo = listaDiaVendas periodo


-- função que ordena uma lista de inteiros
ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (a:x) = insereElementoLista a (ordenaLista x)

-- função que insere um elemento em uma lista ordenada
insereElementoLista::Int->[Int]->[Int] 
insereElementoLista a [] = [a]
insereElementoLista a (b:c) 
  |a>b = b:(insereElementoLista a c)
  |otherwise = a:(b:c)

-- função que ordena lista de lista pelo primeiro elemento

ordenaListaLista::[[Int]]->[[Int]]
ordenaListaLista [] = []
ordenaListaLista ((a:x):y) = insereListaLista (a:x) (ordenaListaLista y)

-- função que insere lista de inteiro uma lista de lista ordenada
insereListaLista::[Int]->[[Int]]->[[Int]] 
insereListaLista (a:x) [] = [a:x]
insereListaLista (a:x) ((b:y):c) 
  |a>b = (b:y):(insereListaLista (a:x) c)
  |otherwise = (a:x):((b:y):c)


{- 02 função que retorna [[Int]] com listas de dia e venda -}

listaDiaVendas::Int->[[Int]]
listaDiaVendas (-1) = []
listaDiaVendas d = [d, vendas d]:listaDiaVendas (d-1)  
  
----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) = ordenaLista [y | y <- xs, y <= x] ++ [x] ++ ordenaLista [y | y <- xs, y > x]

-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
ordenaListaLista :: [[Int]] -> [[Int]]
ordenaListaLista [] = []
ordenaListaLista (x:xs) = 
  ordenaListaLista [y | y <- xs, head y <= head x] ++ [x] ++ ordenaListaLista [y | y <- xs, head y > head x]

---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
ordenaLILE :: [[Int]] -> [[Int]]
ordenaLILE xs = ordenaListaLista [ordenaLista x | x <- xs]

-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
--listaTuplaDiaVenda :: Int-> [(Int, Int)]
listaTuplaDiaVenda :: Int-> [(Int, Int)]
listaTuplaDiaVenda (-1) = []
listaTuplaDiaVenda d = (d, vendas d):listaTuplaDiaVenda (d-1)

{- 07 função que gera o total de vendas-}
totalVendasT :: [(Int, Int)] -> Int
totalVendasT [] = 0
totalVendasT ((_, v):xs) = v + totalVendasT xs

{- 08 função que retorna a maior venda -}
maiorVendaT::Int->[(Int, Int)] -> Int
maiorVendaT mv [] = mv
maiorVendaT mv ((a,b):x) = maiorVendaT (maxi mv b) x
  
{- 08-b como implementar com apenas os parâmetros?  -}
maiorVendaT' :: [(Int, Int)] -> Int
maiorVendaT' [] = 0
maiorVendaT' [(_, v)] = v
maiorVendaT' ((_, v):xs) = max v (maiorVendaT' xs)

{- 09 função que retorna os dias das maiores vendas -}
diasMaiorVenda :: [(Int, Int)] -> [Int]
diasMaiorVenda xs = [d | (d, v) <- xs, v == maiorVendaT' xs]
