-- DEFININDO NOVOS TIPOS (FUNÇÃO TYPE)
type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome, Idade, Linguagem)
pessoa :: Pessoa
pessoa = ("Joao", 20, "Haskell")

main :: IO ()
main = print(pessoa)


my_fst :: Pessoa -> Nome
my_fst (n, i, l) = n
-- main = print(my_(pessoa))