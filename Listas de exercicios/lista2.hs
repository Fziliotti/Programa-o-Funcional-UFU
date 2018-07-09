-- EXEMPLOS DE SOMA
soma :: [Int]-> Int
soma [] = 0
soma (x:xs) = x + soma xs


-- SOMA RECURSIVA DE CAUDA é MAIS EFICIENTE, PARECE UM 'FOR'
soma1 :: [Int]-> Int-> Int
soma1 [] s = s
soma1 (x:xs) s = soma1 xs (x+s)

-- EX 1 Recursão
listaIntervalo :: Int -> Int -> [Int]
listaIntervalo a b
  |a>b = []
  |a==b = [a]
  |a<b = a:listaIntervalo (a+1) b

-- EX 1 Compreensao
listaIntervalo1 :: Int->Int->[Int]
listaIntervalo1 a b = [x | x <- [a..b]]

-- EX 2 Recursao
repetirElemento :: [Int] -> [Int]
repetirElemento [] = []
repetirElemento (x:xs) = x:x:repetirElemento xs

-- EX 3
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- divisores2 :: Int -> Int -> [Int]

-- EX 12
listaL1  = [ x| x <- [100..300], x `mod` 3 ==0]



-- EX 12
-- pega todos os numeros que sao fatores de n
fatores n = [i | i<-[1..n], n `mod` i == 0]
-- verifica se só existem os fatores 1 e n
primo n = if (fatores n) == [1,n] then True else False







