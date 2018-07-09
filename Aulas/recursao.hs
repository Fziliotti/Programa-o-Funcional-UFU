fatorial :: Int -> Int
fatorial n
  |n == 0 = 1
  |otherwise = n * fatorial(n-1)

--casamento padrao
fatorial'  0 =1
fatorial' n = n * fatorial' (n-1)

fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
