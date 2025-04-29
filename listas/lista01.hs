-- questão 1
f1 :: Float -> Float
f1 x
    | x >= 0 = (x+4)/(x+2)
    | otherwise = 2/x

f2 :: Float -> Float -> Float
f2 x y 
    | x >= y = x+y
    |otherwise = x-y

f3 :: Float -> Float -> Float
f3 x y z
    | (x+y) > z = x+y+z
    | (x+y) == z = 0
    | otherwise = x-y-z

-- questão 2
fat::Int->Int
fat 0 = 1 -- caso base faltou, fatorial de 0 é 1
fat x = x * fat(x-1)

-- questão 3
soma :: Int -> Int -> Int
soma x y = (x + y)

multiplica :: Int -> Int -> Int
multiplica _ 0 = 0 --qualquer num multi por 0
multiplica x y
  | y > 0     = soma x (multiplica x (y - 1)) --somar x a ele mesmo y vezes. A cada chamada, y diminui 1, e vamos acumulando o valor com soma.
  | y < 0     = negate (multiplica x (negate y)) --y é negativo, garantindo que a recursão aconteça com valores positivos, mas o resultado final seja negativo
  --negate: É uma função nativa de Haskell. Ela inverte o sinal de um número.

-- questão 4
invertInt :: Int -> Int 
invertInt 0 = 0
invertInt (x:xs) = invertInt xs ++ [x]

-- questão 5
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower y = square (square y)

-- questão 6
i :: Int -> Float
i 0 =sqrt(6)
i n = sqrt((6+i(n-1)))
-- sqrt calcula a raiz quadrada

-- Função para calcular o fatorial de um número
fatorial' :: Integer -> Integer
fatorial' 0 = 1
fatorial' n = n * fatorial (n - 1)

-- Função para calcular o número de combinações
combinacoes :: Integer -> Integer -> Integer
combinacoes m n
  | m >= n = fatorial' m `div` (fatorial' n * fatorial' (m - n))
  | otherwise = error "m deve ser maior ou igual a n"

-- questão 8
mdc :: Int -> Int -> Int
mdc m n
    | m == 0 = n
    | n == 0 = m
    | otherwise = mdc m ( n `mod` m)

-- questão 10
lastDigit :: Int - > Int
lastDigit n = n `mod` 10

-- questão 11
anyDigitAux :: Int->[a]->[a]
anyDigitAux 0 (x:_) = [x]
anyDigitAux x (a:b) = anyDigitAux (x-1) b

anyDigit :: Int->Int->Int
anyDigit x y = read (anyDigitAux x (show y))

-- questão 12
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = ( m/=n) && (n/=p) && (p/=m) 

-- questão 13
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c 
    | (a == b ) && ( b == c) = 3
    | ( a /= b ) && (b /= c ) && ( a /= c) = 0
    | otherwise = 2  

-- questão 14
periodo::Int
periodo = 7

sales :: Int->Int
sales 1 = 41
sales 2 = 72
sales 3 = 48
sales 4 = 2
sales 5 = 91
sales 6 = 55
sales 7 = 30
sales _ = -1

-- a) howManyLess valor comeco fim retorno
howManyLess :: Int->Int->Int->Int
howManyLess x a b
    | a > b || sales a == -1 = 0
    | sales a < x = 1 + howManyLess x (a+1) b 
    | otherwise = howManyLess x (a+1) b

-- b) Se há zeros no periodo
noZerosInPeriod :: Int->Bool
noZerosInPeriod a 
    | sales a == -1 = True
    | sales a == 0 = False 
    | otherwise = noZerosInPeriod (a+1)


-- c) Quantidade de vendas iguais a zero
zeroInPeriod :: [Int]
zeroInPeriod = zerosPeriodToList 1

zerosPeriodToList :: Int->[Int]
zerosPeriodToList a
    | sales a == -1 = []
    | sales a == 0 = zerosPeriodToList (a+1) ++ [a]
    | otherwise = zerosPeriodToList (a+1)

-- d) Retorna lista de dias que a venda foi menor que um parametro
daysLassThan :: Int->[Int]
daysLassThan a = daysLassThanToList a 1

daysLassThanToList :: Int->Int->[Int]
daysLassThanToList x a 
    | sales a == -1 = []
    | sales a < x = daysLassThanToList x (a+1) ++ [a]
    | otherwise = daysLassThanToList x (a+1)

-- questão 15
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

antiFib :: Int -> Int
antiFib x = antiFibAUX x 0

antiFibAUX :: Int -> Int -> Int
antiFibAUX x y
    | x == fib y = y
    | x > fib y = antiFibAUX x (y+1)
    | x < fib y = -1

-- questão 16
funny :: Int -> Int -> Int -> Bool
funny x y z = (x > z) && (y < x)

-- questão 17 - letra minuscula para maiuscula
paraMaiuscula :: Char -> Char
paraMaiuscula x
  | ord x >= ord 'a' && ord x <= ord 'z' = chr (ord x - (ord 'a' - ord 'A'))
  | otherwise = x
-- letra maiuscula para minuscula
import Data.Char
paraMinuscula :: Char -> Char
paraMinuscula x
  | ord x >= ord 'A' && ord x <= ord 'Z' = chr (ord x + (ord 'a' - ord 'A'))
  | otherwise = x

-- questão 18
charToNum::Char->Int
charToNum x 
    | isDigit x = ord x - ord '0' -- se for digito
    | otherwise = -1  

-- questão 19
duplicate :: String -> Int -> String
duplicate s n 
    | n == 0 = ['-1']
    | n == 1 = s
    | otherwise = s ++ duplicate s (n-1) 

-- questão 20
compS :: Int -> Int
compS [] = 0
compS (_:xs) = 1 + contaS xs  

pushRight :: String -> Int -> String
pushRight s n 
    | n < contaS s = s
    | otherwise = addG (n- compS s) ++ s -- n é o novo comprimento total!

addG :: Int -> String
addG 0 = ""
addG k = '>' : addG (k-1)

-- questão 21 ???

-- questão 22 
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- questão 22
separa :: [Int] -> ([Int], [Int])
separa [] = ([], [])
separa (x:xs)
    | x `mod` 2 == 0 = (x : pares, impares)
    | otherwise      = (pares, x : impares)
  where
    (pares, impares) = separa xs

-- questão 23
converte [] = ""
converte (x:xs) = chr (x + ord 'A' - 1) : converte xs

-- questão 26
ehA :: [Char] -> Char -> Int
ehA [] a = 0
ehA (x:xs) a 
    | ord x == ord a = 1 + ehA xs a
    | otherwise = ehA xs a

-- questão 27
purifica :: [Int] -> [Int]
purifica [] = []
purifica (x:xs)= true pertence xs
    | pertence x xs = purifica xs
    | otherwise = x : purifica xs

pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence a (x:xs)
    | a == x = true
    | otherwise = pertence a xs

-- questão 28
