-- TUPLAS - conj de dados heterogenios
func :: (Int, Int) -> (Int, Int) -> (Int, Int)
func (a,b) (c,d) = (a+c, b+d)

main :: IO ()
main = print (func (1,2)(2,4))