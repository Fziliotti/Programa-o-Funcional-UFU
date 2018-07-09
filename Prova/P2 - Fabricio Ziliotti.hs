collatz :: Int -> [Int]
collatz x
           | x==1 = [1]
           | even x = (x : collatz (div x 2))
           | otherwise = (x : collatz ((3*x) + 1))

outraCollatz :: Int -> [Int] -> [Int]
outraCollatz x lista
                 | x == 1 = lista ++ [1]
                 | even x = outraCollatz (div x 2) (lista ++ [x])
                 | otherwise = outraCollatz (3*x + 1) (lista++[x])




-----------------------------------------------------------

-- explode :: Int -> [Int]
-- explode x = (reverse.map (`mod` 10).takeWhile (>0).iterate ( `div` 10 )) x 

explode :: Int -> [Int]
explode x
  |x == 0 = [0]
  |otherwise = result x
  	where result = reverse.map(`mod` 10).takeWhile(>0).iterate( `div` 10)
-----------------------------------------------------------

data Par a b= Par2 a b deriving (Eq)

instance (Ord a, Ord b) => Ord(Par a b) where
    (<=) (Par2 a b) (Par2 x y) = if(a == x) then (b<=y)
                               else a<x  


data Lista a = Lista2 [a] deriving (Eq)

instance (Ord a) => Ord(Lista a) where
    (<=) (Lista2 x) (Lista2 y)
                               | (Lista2 x) == Lista2 [] = True
                               | (Lista2 y) == Lista2 [] = False
                               | (head x) > (head y)  = False
                               | (head x) < (head y) = True
                               | otherwise  = (<=) (tail x) (tail y)