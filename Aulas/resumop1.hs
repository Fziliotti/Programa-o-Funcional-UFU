
-- b é uma variavel Coringa
e_logico :: (Bool,Bool) -> Bool
e_logico (True, b) = b
e_logico (False, _)= False

triplo x = 3*x

concatenar :: [[a]] -> [a]
concatenar l = [x | xs <- l, x <-xs]

mdc :: (Int,Int) -> Int
mdc (m,n)
  |n==0 = m
  |otherwise = mdc (n,mod m n)

primeiroElem :: [a] -> a
primeiroElem (x:_) = x

ultimoElem :: [a] -> a
ultimoElem (a:[]) = a
ultimoElem (x:xs) = ultimoElem(xs)


primeiros:: Int -> [t] ->[t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) =x: primeiros (n-1) xs

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x:xs)
  |a==x = True
  |otherwise = pertence a xs

uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [b | b <- bs, not(pertence b as)]
  

n_esimo :: Int ->[a] -> a
n_esimo _ [] = error "DIGITA CERTO!"
n_esimo 1 (x:xs) = x
n_esimo n (x:xs) = n_esimo (n-1) xs

n_esimo2 :: Int ->[a] -> a
n_esimo2 _ [] = error "DIGITA CERTO!"
n_esimo2 1 (x:_) = x
n_esimo2 n (_:xs) = n_esimo2 (n-1) xs



fatores :: Int -> [Int]
fatores n = [ x| x<- [1..n], x `mod` n == 0 ]

numElementos xs = sum [ 1 | _ <- xs]

-- primo :: Integral a => a -> Bool
-- primo x  
--   |fatores x == [1,x] = True
--   |otherwise = False

combina::[Int]->Int->[[Int]]
combina [] _ = [[]]
combina _ 0 = [[]]
combina (a:b) n = [i:rest | i <- (a:b) , rest <- combina (a:b) (n-1) ]


--eh uma PA com razao r e que vai de a1 ate 100
progArit r a1 = [a1,(a1+r)..100]




--quicksort colocando o primeiro elemento como o pivo
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a<-xs, a<x]
                   ++ [x] ++
                   quicksort [b | b<-xs, b>=x]


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
	let smallerOrEqual = [a | a <- xs, a <= x]
	    larger = [a | a <- xs, a > x]
	in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger


repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos (x:xs) = x:x:repetidos xs 


maior [x] = x
maior (x:y:resto)
  |x>y = maior (x:resto)
  |otherwise = maior (y:resto)


