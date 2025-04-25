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
iesimoTermo :: Int -> Double
iesimoTermo i = fromIntegral (i + 1) * sqrt 6

-- questão 8
mdc :: Int -> Int -> Int
mdc m n
    | m == 0 = n
    | n == 0 = m
    | otherwise = mdc m ( n `mod` m)

-- questão 10
lastDigit :: Int - > Int
lastDigit n = n `mod` 10

-- questão 13
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c 
    | (a == b ) && ( b == c) = 3
    | ( a /= b ) && (b /= c ) && ( a /= c) = 0
    | otherwise = 2  

-- questão 16
funny :: Int -> Int -> Int -> Bool
funny x y z = (x > z) && (y < x)

-- questão 22 
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- 
