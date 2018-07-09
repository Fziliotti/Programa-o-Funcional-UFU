soma10 :: Int -> Int -> String
soma10 x y
  |somatoria > 5 = "Somatoria maior que 5!"
  |somatoria > 10 = "Soma maior que 10!"
  |otherwise = "algo esta errado!"
  where somatoria = x+y


primeiroElem:: [Int]->String  
primeiroElem [] = "Esta errado!"
primeiroElem (x:_) = "O primeiro elemento da lista eh: " ++ show x


pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z)
  |(a == x) = True
  |otherwise = pertence a z


mult3  = [3*x | x <- [100..300]]

-- FUNCAO DE ALTA ORDEM MAP -> UMA FUNCAO APLICADA EM CADA ELEMNTO DA LISTA
dobro:: Num a => [a] -> [a]
dobro [] = []
dobro (x:xs) = 2*x : dobro xs

-- map :: (a->b) -> [a] -> [b] 
-- map f [] = []
-- map f (x:xs) = f x : map f xs

triplo :: Num x => x ->x
triplo x = 3*x

mapeamento = map (even) [1,2,3,4,12,13]



