-- Calculo Lambda

soma' :: Int -> (Int ->Int )
soma' x = terminaSoma
  where terminaSoma y = x+y


soma'' :: Int -> (Int ->Int )
soma'' x = \y -> x+y

soma''' :: Int -> (Int ->Int )
soma''' = \x -> (\y -> x+y)

teste = (\x -> 2*x) 3