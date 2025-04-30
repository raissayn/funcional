import Data.Char

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
proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (x:xs)
    | x <= 0 = proliferaInt xs
    | otherwise = replica x  ++ proliferaInt xs

replica :: Int -> [Int]
replica n
    | n <= 0 = []
    | otherwise = n : replica (n-1)

-- questão 29
proliferaChar :: [Char] -> [Char]
proliferaChar [] = []
proliferaChar (x:xs) = replica (ordem x) x ++ proliferaChar xs

-- Calcula a ordem da letra no alfabeto
ordem :: Char -> Int
ordem c = fromEnum c - fromEnum 'A' + 1

-- Repete o caractere n vezes (sem usar replicate)
replica :: Int -> Char -> [Char]
replica n c
    | n <= 0 = []
    | otherwise = c : replica (n-1) c

-- questão 30

-- Exercicio 31: Funcoes de rg
pessoa :: Int -> (String, Int, Char)
pessoa rg
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Maria Oliveira", 34, 'f')
    | rg == 4 = ("Ana Costa", 28, 'f')
    | rg == 5 = ("Carlos Pereira", 45, 'm')
    | rg == 6 = ("Fernanda Lima", 19, 'f')
    | rg == 7 = ("Paulo Santos", 60, 'm')
    | rg == 8 = ("Juliana Alves", 25, 'f')
    | rg == 9 = ("Ricardo Mendes", 33, 'm')
    | rg == 10 = ("Beatriz Rocha", 40, 'f')
    | otherwise = ("Não há ninguém mais", 9999, 'x')

prm :: (a,b,c) -> a
prm (x,_,_) = x

sec :: (a,b,c) -> b
sec (_,x,_) = x

trc :: (a,b,c) -> c
trc (_,_,x) = x



-- a) pessoa de menor de idade ate determinado registro
menorIdade :: Int -> String
menorIdade x = menorIdadeAUX x x

menorIdadeAUX :: Int -> Int -> String
menorIdadeAUX 0 x = prm (pessoa x)
menorIdadeAUX n x
    | sec (pessoa n) < sec (pessoa x) = menorIdadeAUX (n-1) n
    | otherwise = menorIdadeAUX (n-1) x

-- b) Idade media das pessoas ate determinado registro
idadeMedia :: Int -> Int
idadeMedia 0 = 0
idadeMedia x = (somaIdades x) `div` x

somaIdades :: Int -> Int
somaIdades 0 = 0
somaIdades x = sec (pessoa x) + somaIdades (x-1)

-- c) numero de pessoas do sexo masculino
numDeMasc :: Int
numDeMasc = numDeMascAUX 1

numDeMascAUX :: Int -> Int
numDeMascAUX n 
    | trc (pessoa n) == 'm' = 1 + numDeMascAUX (n+1)
    | trc (pessoa n) == 'f' = numDeMascAUX (n+1)
    | trc (pessoa n) == 'x' = 0
    | otherwise = numDeMascAUX (n+1)

-- d) Numero do registro da pessoa de maior idade
maiorIdade :: Int
maiorIdade = maiorIdadeAUX 1 1

maiorIdadeAUX :: Int -> Int -> Int
maiorIdadeAUX x y
    | trc (pessoa x) == 'x' = y
    | sec (pessoa x) > sec (pessoa y) = maiorIdadeAUX (x+1) x
    | otherwise = maiorIdadeAUX (x+1) y

-- questão 32
ordena :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordena a b c d
    | a <= b && a <= c && a <= d = (a, menor2 b c d)
    | b <= a && b <= c && b <= d = (b, menor2 a c d)
    | c <= a && c <= b && c <= d = (c, menor2 a b d)
    | otherwise                  = (d, menor2 a b c)

menor2 :: Int -> Int -> Int -> (Int, Int, Int)
menor2 x y z
    | x <= y && x <= z = (x, menor3 y z)
    | y <= x && y <= z = (y, menor3 x z)
    | otherwise        = (z, menor3 x y)

menor3 :: Int -> Int -> (Int, Int)
menor3 x y
    | x <= y = (x, y)
    | otherwise = (y, x)

-- questão 33 ???

-- questão 34 - Equação de Segundo grau
equacao :: (Float,Float,Float) -> (Float,Float)
equacao (a,b,c) 
    | (b*b) - (4*a*c) < 0 = error "Raiz negativa"
    | otherwise = (((-b) + sqrt((b*b) - (4*a*c))) / (2*a),
                   ((-b) - sqrt((b*b) - (4*a*c))) / (2*a))



-- questão 36: Base de professores
base :: Int -> (Int, String, String, Char)
base x
    | x == 0  = (1452, "Luciana Prado", "DOUTOR", 'F')
    | x == 1  = (1903, "Ricardo Moreira", "MESTRE", 'M')
    | x == 2  = (1601, "Juliana Costa", "MESTRE", 'F')
    | x == 3  = (1789, "Bruno Mendes", "DOUTOR", 'M')
    | x == 4  = (1333, "Helena Barbosa", "DOUTOR", 'F')
    | x == 5  = (1740, "Fábio Santos", "MESTRE", 'M')
    | x == 6  = (1855, "Carla Ferreira", "MESTRE", 'F')
    | x == 7  = (1499, "Tiago Lima", "DOUTOR", 'M')
    | x == 8  = (1711, "Gabriela Nogueira", "MESTRE", 'F')
    | x == 9  = (1670, "Eduardo Matos", "DOUTOR", 'M')
    | x == 10 = (0, "", "", '0')
    | otherwise = (0, "", "", '0')


prm4 :: (a,b,c,d) -> a
prm4 (x,_,_,_) = x

sgd4 :: (a,b,c,d) -> b
sgd4 (_,x,_,_) = x

trc4 :: (a,b,c,d) -> c
trc4 (_,_,x,_) = x

qrt4 :: (a,b,c,d) -> d 
qrt4 (_,_,_,x) = x

-- a) Numero de doutores na base
qtdDoutores :: Int
qtdDoutores = qtdDoutoresAUX 0

qtdDoutoresAUX :: Int -> Int
qtdDoutoresAUX x 
    | prm4 (base x) == 0 = 0
    | trc4 (base x) == "DOUTOR" = 1 + qtdDoutoresAUX (x+1)
    | otherwise = qtdDoutoresAUX (x+1)

-- b) Numero de mulheres
qtdMulheres :: Int
qtdMulheres = qtdMulheresAUX 0

qtdMulheresAUX :: Int -> Int
qtdMulheresAUX x
    | prm4 (base x) == 0 = 0
    | qrt4 (base x) == 'F' = 1 + qtdMulheresAUX (x+1)
    | otherwise = qtdMulheresAUX (x+1)

-- c) Numero de Mestres do sexo Masculino
qtdMestreMasc :: Int
qtdMestreMasc = qtdMestreMascAUX 0

qtdMestreMascAUX :: Int -> Int
qtdMestreMascAUX x 
    | prm4 (base x) == 0 = 0
    | trc4 (base x) == "MESTRE" && qrt4 (base x) == 'M' = 1 + qtdMestreMascAUX (x+1)
    | otherwise =  qtdMestreMascAUX (x+1)

-- d) Nome do professor mais antigo
profMaisAntigo :: String
profMaisAntigo = sgd4 (base (profMaisAntigoAUX 0 0))

profMaisAntigoAUX :: Int -> Int -> Int 
profMaisAntigoAUX x y
    | prm4 (base x) == 0 = y
    | prm4 (base x) < prm4 (base y) = profMaisAntigoAUX (x+1) x
    | otherwise = profMaisAntigoAUX (x+1) y

-- questão 37: 
type Acervo = [(Isbn, Titulo, Reserva, Volumes)]
type Emprestimo = [(Matricula, Isbn)]

type Isbn = Int -- Isbn de um livro
type Volumes = Int -- quantidade no acervo
type Titulo = String -- título do livro
type Matricula = String -- matrícula do discente
type Reserva = Bool -- deve permanecer na biblioteca?

acervo :: Acervo
acervo = [
    (1, "Haskell Básico", False, 3),
    (2, "Programação Funcional", True, 2),
    (3, "Estruturas de Dados", False, 1),
    (4, "Compiladores", False, 0)
    ]

emprestimo :: Emprestimo
emprestimo = [
    ("2023001", 1),
    ("2023002", 1),
    ("2023003", 3)
    ]

-- a) Reservado = True, 
func_1::Isbn-> Acervo -> Bool
func_1 _ [] = error "Isbn nao encontrado"
func_1 x (a:as) 
    | x == prm4 a = not (trc4 a)
    | otherwise = func_1 x as

-- b) quantidade de livros emprestado
func_2::Isbn -> Emprestimo -> Int
func_2 _ [] = 0
func_2 x (a:as)
    | x == (snd a) = 1 + (func_2 x as)
    | otherwise = func_2 x as

-- c) quantidade de livros no acervo
func_3::Isbn -> Acervo -> Int
func_3 _ [] = 0
func_3 x (a:as)
    | x == prm4 a = qrt4 a
    | otherwise = func_3 x as

-- d) quantidade de livros disponiveis para emprestimo
func_4 :: Isbn -> Int
func_4 x
    | (func_3 x acervo) == (func_2 x emprestimo) || not (func_1 x acervo) = 0
    | otherwise = (func_3 x acervo) - (func_2 x emprestimo)

-- e) recebe a matricula e o isbn e ve se pode pegar ou nao o livro
func_5 :: Matricula -> Isbn -> Emprestimo
func_5 mat isbn 
    | (func_4 isbn) /= 0 = (mat,isbn) : emprestimo  
    | otherwise = emprestimo 