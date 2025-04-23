
type VendaR = Int

-- define o período de recursão
periodo::Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 2
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30
vendas _ = 0

totalVendas :: Int -> Int
totalVendas 1 = vendas 1
totalVendas x = vendas x + totalVendas (x-1)

tVendas:: Int
tVendas = totalVendas periodo

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise	= n

maiorVenda:: Int->Int
maiorVenda 1 = vendas 1
maiorVenda x = maxi (vendas x) (maiorVenda (x -1)) 

maiorVenda2::Int->Int->Int
maiorVenda2 0 y = y
maiorVenda2 x y
  |vendas (x-1) > y = maiorVenda2 (x-1) (vendas (x-1))
  |otherwise        = maiorVenda2 (x-1) y

maiorVenda3::Int->Int->Int
maiorVenda3 0 y = y
maiorVenda3 x y = maiorVenda3 (x-1) (maxi y (vendas x)) 


vendaRelevante2 :: (Int->Int->Bool) -> Dia -> VendaR -> Int
vendaRelevante2  _  0  vendaR = vendaR
vendaRelevante2 f dia vendaR 
   | f vendaR (vendas dia)  = vendaRelevante2 f (dia-1) vendaR
   | otherwise = vendaRelevante2 f (dia-1) (vendas dia) 


vendaRelevante :: (Int->Int->Int) -> Dia -> VendaR -> Int
vendaRelevante _  0  vendaR = vendaR
vendaRelevante f dia vendaR = vendaRelevante f (dia-1) (f vendaR (vendas dia))

-- função interface usuario para vendaRelevante 
vendaRmm :: (Int->Int->Int)->Int
vendaRmm f = vendaRelevante f periodo (vendas periodo)


-- função auxiliar maior
maior::Int->Int->Int
maior a b
  | (a>b) = a
  | otherwise = b
  
-- função auxiliar menor
menor::Int->Int->Int
menor a b
  | (a<b) = a
  | otherwise = b  




-- função que retorna o total de vendas
-- função que retorna quantas vendas superam um valor


