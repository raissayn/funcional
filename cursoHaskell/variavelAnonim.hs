-- VARIAVEL ANÔNIMA
andi :: Bool -> Bool -> Bool 
andi False _ = False
andi _ False = False
andi True True = True

main :: IO ()
main = print (andi False True)