{-Objetivos: introduzir os conceitos de
   a) List Comprehension
   b) Função de alta ordem.
   
   Para tanto, iniciamos com um problema simples e mostramos,
   a cada passo, as possibilidades de melhorias.
   -}
import Data.Char

{- faça f1 capaz de somar uma lista de inteiros se um Char for alfanumérico, 
    ou multiplicar os elementos, caso contrário -}
f1::Char->[Int]->Int
f1 c x
  |isDigit (c)     && x==[]    = 0
  |not (isDigit c) && x==[]    = 1
  |isDigit c                   = a + f1 c b
  |otherwise                   = a * f1 c b
    where (a:b) = x

    
{- reescreva f1 usando casamento de padrão -}
f2::Char->[Int]->Int
f2 c x   = f2_aux (isDigit c) x

f2_aux::Bool->[Int]->Int
f2_aux True   []   = 0
f2_aux False  []   = 1
f2_aux True  (a:b) = a + f2_aux True b
f2_aux False (a:b) = a * f2_aux False b

{- reescreva f2 fazendo chamadas de funções para somar ou multiplicar -}
f3::Char->[Int]-> Int
f3 c x = f3_aux (isDigit c) x

f3_aux:: Bool->[Int]->Int
f3_aux True  x = somaL x
f3_aux False x = multL x

somaL []    = 0
somaL (a:b) = a + somaL b

multL []    = 1
multL (a:b) = a * multL b

{- reescreva f3 usando função de alta ordem
   Esta função é didática, pois mostra o uso de função de alta ordem
   Contudo, o booleano não seria necessário se conseguíssemos fazer casamento 
   de padrão com a função parâmetro-}
--f4::Char->(Int->Int->Int)->[Int]->Int
--f4 c op x = f4_aux (isDigt c) op x
 
--f4_aux :: Bool->(Int->Int->Int)->[Int]->Int
 {- para lista de pelo menos um elemento -}
f4::(Int->Int->Int)->[Int]->Int
f4 op [a] = a
f4 op (a:x) = (op) a (f4 op x) 

{- soma ou subtrai elementos de lista com função de alta ordem -}
sumLsubL :: (Int->Int->Int) -> [Int] -> Int
sumLsubL op [] = 0
sumLsubL op (a:x) = (op) a (sumLsubL op x)

{- função map aplica uma função a cada elemento de uma lista -}

{- função que converte caixa baixa para caixa  alta
   usar a função map para aplicar a uma String -}
   
intervalo = ord ('a') - ord ('A')
caixaBaixaAlta c = chr (ord c - intervalo)

