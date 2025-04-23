-- EXTRAINDO DADOS DE TUPAS
nomes :: (String, String, String)
nomes = ("Raissa", "Amanda","João")

selec_prim (x, _, _) = x
selec_sec ( _, y, _) = y
selec_ter ( _, _, z) = z

main :: IO ()
main = print(selec_prim nomes)