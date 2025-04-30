type Dia = Int
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
totalVendas 0 = 0 
totalVendas d = f d + totalVendas (d-1)
------------------------------------------------------------------
{-encontra o dia em que mais se vendeu no período - versão 01-}
diaMaiorVenda :: Int -> Int 
diaMaiorVenda 1 = 1 --se só tiver um dia é ele mesmo
diaMaiorVenda d 
    | f d >= f (diaMaiorVenda(d-1)) = d 
    | otherwise = (diaMaiorVenda (d-1))

------------------------------------------------------------------
{-encontra o dia em que mais se vendeu no período - versão 02
  tem como parâmetros o período e a maior venda -}
  
maiorVenda02::Int->Int->Int
maiorVenda02 0 y = y
maiorVenda02 x y
  |f (x-1) > y = maiorVenda02 (x-1) (f (x-1))
  |otherwise  = maiorVenda02 (x-1) y

-------------------------------------------------------------------

{- encontra o maior valor entre dois inteiros -}
maxi :: Int -> Int -> Int
maxi a b
    | a > b = a
    | otherwise = b
--------------------------------------------------------------------
{- encontra a maior venda - versão 01 
   Exercício: implemente essa função com apenas dois parâmetros e 
   fazendo uso de maxi no código interno-}
maiorVenda :: Int -> Int -> Int
maiorVenda 0 y = y
maiorVenda d y = maiorVenda (x-1) (maxi (f (x-1)) y) 

----------------------------------------------------------------------
{- encontra a maior venda - versão 02 -}
maiorVenda02::Int->Int->Int
maiorVenda02 0 y = y
maiorVenda02 x y
  |f (x-1) > y = maiorVenda02 (x-1) (f (x-1))
  |otherwise  = maiorVenda02 (x-1) y

-----------------------------------------------------------------------

{- media das vendas -}
mediaVendas :: Int -> Float
mediaVendas d = fromIntegral (totalVendas d) / fromIntegral d

