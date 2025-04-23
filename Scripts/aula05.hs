import Data.Char

{- faça a função f1 que receba uma String S
e Retorne uma String R.
R deve ser igual a S, com exceção de que
todo caracter alfanumérico x encontrado em S
que esteja seguido de um caracter y não alfanumérico
determinará que y será repetido x vezes em R

exemplo  f1 "ab42c570sd3f" retorna "ab42cc570d3fff" 
-}

{- Observação. No Data.Char existe 
            isDigit::Char->Bool -}


--função f1
replica :: Int -> Char -> String
replica 0 _ = []
replica x c = c:repete (x-1) c

num :: Char -> Int
num = digitToInt

f1 :: String -> String -- recebe r e retorna s
f1 [] = [] -- caso base
f1 [a] = [a]
f1 (a:b:x) -- primeiro caractere, segundo caractere e resto da lista
-- se a for um número e b não for um número
    | isDigit a && not(isDigit b) = a:(replica(num a)b) ++ f1 x -- retorna a e replica b a vezes e continua o resto (f1 x)
    | isDigit a && isDigit b      = a:f1(b:x) -- se a e b forem números então repete 
    | otherwise                   = a:f1(b:x) -- se a não for digito, mantém e continua


{- faça f11, outra versão de f1, que retorne R do tipo [(Char, Bool, Int)] 
de modo que, para cada caractere de S, informe se ele será repetido ou não
e a quantidade de vezes. 
 (caractere, seEleSeráRepetido, quantasVezes)
Por exemplo, f11 "ab42c570sd3f" retorna [('a',False,1),('b',False,1),
('4',False,1),('2',False,1),('c',True,51),('5',False,1),('7',False,1),
('0',False,1),('s',True,51),('d',False,1),('3',False,1),('f',True,51)] -}

f11::String->[(Char, Bool, Int)]
f11 [ ]      = []       -- caso base: string vazia
f11 [a]      = [(a, False, 1)]   -- último caractere da string: nunca é repetido
f11 (a:b:xs)
  | isDigit a && not (isDigit b) = 
      (a, False, 1) : (b, True, ord a) : f11 xs
  | otherwise = 
      (a, False, 1) : f11 (b:xs)

{-Agora, implemente a função f111 que receba [(Char, Bool, Int)] e gere uma String
com os caracteres repetidos ou não (como R em f1). Use o Bool da dupla-}

f111:: [(Char, Bool, Int)] -> String
f111 [] = []
f111 ((c, False, _) : xs) = c : f111 xs         -- se não vai repetir, só coloca uma vez
f111 ((c, True, n)  : xs) = replicate n c ++ f111 xs  
-- se vai repetir, usa replicate: cria uma String com o caractere c repetido n vezes(função padrão do Haskell)

{-faça a função f2 que receba uma lista de
Strings e aplique a todas as strings a 
computação da função f1-}

f2::[String]->[String]
f2 [] = []                               -- caso base: lista vazia
f2 (x:xs) = f1 x : f2 xs                 -- aplica f1 ao primeiro elemento, e chama recursivamente pro resto

{-faça a função f3 que receba uma String S 
e retorne uma dupla de Bool e String.
A string de saída deve ter o caracter
da ordem alfabética das letras minúsculas
por substituição a cada caracter numérico
(1..9) que aparece em S. O bool deve informar se 
a entrada foi ou não alterada.
Ex:  f3 "a2c4x" retorna ("abcdx", True)-}

--f3::String -> (String, Bool)
f3:: String ->(String, Bool)
f3 