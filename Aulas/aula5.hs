
--  compreensao 
eliminaNeg:: [Int]-> [Int]
eliminaNeg [] = []
eliminaNeg (x:xs)
  |x<0 = eliminaNeg xs
  |otherwise = (x:eliminaNeg xs)


eliminaPares:: [Int]-> [Int]
eliminaPares [] = []
eliminaPares (x:xs)
  |even x = eliminaPares xs
  |otherwise = (x:eliminaPares xs)


eliminaNeg' y = [x | x<-y, x > 0]
eliminaPares' y = [x | x <- y, odd x]
eliminaImpares' y = [x | x <- y, even x]

-- FILTER APLICA A FUNCAO EM CADA ELEMENTO, MAS SÓ DEVOLVE O QUE RETORNA TRUE
eliminaPares'' l = filter odd l

teste a b = [(x:y) | x<-a, y<-b]


-- MAP aplica a funcao em cada elemento
-- incrementar :: [Int] -> [Int]
-- incrementar l x = map (+1) l

-- duplicar :: [Int] -> [Int]
-- incrementar l x = map (+1) l












-- teste2 x = (odd) [x]

-- pegaLetras :: String -> String
-- pegaLetras []=[]
-- pegaLetras (x:xs)
--   |isAlpha x = x:pegaLetras xs
--   |otherwise = pegaLetras xs


-- foldr
paresOrdenados a b = [ (x,y) | x <- a, y <- b]
paresOrdenados' a b = sum[ x * y | x <- a, y <- b]


potencia2' :: Integer->Integer->Integer
potencia2' n y
  |n==0 = y
  |n>0 = potencia2' (n-1) (2*y)

-- lowers :: String -> Int
-- lowers xs = length[x | x <- xs, isLower x]


