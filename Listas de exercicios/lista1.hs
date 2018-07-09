-- import Data Char
-- EX 1
triplo :: Double->Double
triplo x = 3*x

-- EX 2
maiorDeTres :: Int->Int->Int->Int
maiorDeTres x y z
    |x>=y && x>=z = x
    |y>=x && y>=z = y
    |otherwise = z

-- menorDeTres :: Int->Int->Int->Int
-- menorDeTres x y z 1
--     |x<=y && x<=z = x
--     |y<=x && y<=z = y
--     |otherwise = z

-- EX 3
somatoria :: Int-> Int
somatoria s = sum [0..s]

somatorio :: Int -> Int
somatorio 0 = 0
somatorio n = n + somatorio (n-1)

somatorio2 :: Int-> Int -> Int
somatorio2 0 x =0
somatorio2 n x = n + somatorio2 (n-1) (x+1)


-- EX 4
nesimoTermoPA ::Double->Double->Double->Double
nesimoTermoPA a1 r n = a1 + (n-1)*r

-- EX 5
nesimoTermoPG :: Int->Int->Int->Int
nesimoTermoPG a1 q n = a1*q^(n-1)

-- -- EX 6
-- somaPA :: Double->Double->Double->Double
-- somaPA n a0 r
--   |(n == 1) = a0
--   |(n /= 1) = somaPA n-1 a0+r r

-- EX 6
somaPA :: Double->Double->Double->Double
somaPA 1 a0 r = a0
somaPA n a0 r = a0 + somaPA (n-1) (a0+r) r

sum_pa :: (Int,Int,Int)->Int
sum_pa(0,a1,r) = 0
sum_pa(n,a1,r) = a1 +sum_pa(n-1,a1+r,r)

-- -- EX 7
-- somaPG :: Double->Double->Double->Double
-- somaPG a1 q n = (a1* (q^n-1)) / (q-1)

-- EX 8 
-- 1 1 2 3 5 8 13 21 34 55
fib :: Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1)+ fib(n-2)

anoBissexto :: Int->String
anoBissexto ano
  |(mod ano 4 == 0)  && (mod ano 100 /= 0) || (mod ano 400 == 0) = "ANO BISSEXTO"
  |otherwise = "NAO EH BISSEXTO"



-- EX 10
-- ehPrimo :: Int->Boolean
-- ehPrimo x
--   |mod x 2 == 0

-- EX 11
ehMaiuscula:: Char -> String
ehMaiuscula s
  |s >= 'A' && s <= 'Z'  = "True"
  |s >= 'a' && s <= 'z' = "False"
  |otherwise = "False"

-- EX 12
ehMinuscula:: Char -> String
ehMinuscula s
  |s >= 'a' && s <= 'z'  = "True"
  |s >= 'A' && s <= 'Z' = "False"
  |otherwise = "False"

-- EX 13
-- isDigit:: char -> Boolean
-- isDigit n
--     | n


-- EX 18
-- teste18 :: Int->Int->Int->[Int]
-- teste18 a b c 
--     | a==b && b==c = [a,b,c]
--     | a>b = [n1,9,n2]
--     where
--     n1 = maiorDeTres a b c
--     n2 = menorDeTres a b c
