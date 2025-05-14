% Aula dada em sala de Aula
% [expressão | geradores, condições]

f1 :: Int -> [Int] -> [Int]
f1 x l = [ x*a | a <- l ] % | = condição
     % dentro do l pode ser lista com elementos ou vazia

f2 :: Int -> [Int] -> [Int]
f2 x l = [a*x | a <- l, (mod) a 2 == 0]

ff2 :: Int -> [Int] -> [Int]
ff2 x [] = []
ff2 x (a:b)
    | ((mod) a 2 == 0) = a*x:f2 x b
    | otherwise = f2 x b

ff3 :: Int -> [Int] -> [Int]
ff3 x (a:b)
    | ((mod) a 2 == 0) == a * x: f2 x b 
    | otherwise = a:f2 x b

f3 :: Int -> [Int] -> [Int]
f3 x l = [x*a | a <- l, (mod a 2 == 0)] ++ [a| a <- l,(mod a 2/= 0)]
% , é &&

f4 :: Int -> [Int] -> [Int]
f4 x l = [x*a | a <- l, ((mod)a 2 == 0) || a > 5] -- lista com elementos multip. por x que são pares ou maiores que 5

f5 :: Int -> [Int] -> [Int]
f5 x l = [x*a | a <- l, ((mod)a 2 == 0) && a > 5] -- ou
% f5 x l = [x*a | a <- l, ((mod)a 2 == 0), a > 5] 