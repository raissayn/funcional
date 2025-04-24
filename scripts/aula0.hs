import Data.Char

type Dia = Int
type Venda = Int

periodo::Int
periodo = 7

answer :: Int
answer = 42

square :: Int -> Int
square x = x * x

soma::Int->Int->Int
soma z k = z+k

allEqual :: Int -> Int -> Int -> Bool
allEqual m n p = (m==n) && (n==p)

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise	= n


f :: Dia -> Venda
f 1 = 9
f 2 = 7
f 3 = 15
f 4 = 14
f 5 = 8
f 6 = 0
f 7 = 3
f x = -1

{- função que retorne a maior venda da semana -}
maiorVenda::Int->Int->Int
maiorVenda 0 v = v
maiorVenda d v
  |f d > v = maiorVenda (d-1) (f d)
  |otherwise = maiorVenda (d-1) (v)

maiorV7 :: Int
maiorV7 = maiorVenda periodo 0

{- Exercícios:
   implemente uma função que retorne o dia em que houve a maior venda (função f)-}
   
{- implemente uma função que retorne a quantidade de vendas do período -}

{- implemente uma função que retorne a média de vendas-}   


vendaDia :: Int -> Int -> Dia
vendaDia 0 _ = 0
vendaDia d maiorDia
    | f d > f maiorDia = vendaDia (d-1) d
    | otherwise = vendaDia (d-1) maiorDia

diaMaiorV7 :: Dia
diaMaiorV7 = diaMaiorVenda periodo 1

totalVendas :: Int -> Int -> Int
totalVendas 0 total = total
totalVendas d total = totalVendas (d-1) (total + f d)

vendasTotais :: Int
vendasTotais = quantidadeVendas periodo 0

mediaVendas :: Float
mediaVendas = fromIntegral (vendasTotais) / fromIntegral periodo