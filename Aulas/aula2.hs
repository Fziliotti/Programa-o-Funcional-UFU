

zeroto :: Int-> [Int]
zeroto n = [0..n]

listapar :: Int-> [Int]
listapar n = [0,2..n]

listamenos2 :: Int-> [Int]
listamenos2 n = [n,n-2..0]

teste :: Int-> [Int]
teste n = [0,2..n]

fatorial n = product [1..n]

isDigit :: Char->Bool
isDigit c
  |c >= '0' && c <= '9' = True
  |otherwise = False

ehAlpha n = not (isDigit n)



par n = mod n 2==0 
impar n = not (par n)

absolute ::Int->Int
absolute n = if n>=0 then n else -(n)

-- signum ::Int->Int 
-- signum n = if n<0
--     then -1
-- 	else if n==0
-- 		then 0
-- 		else 1

-- signum n |n<0 = -1
--          |n==0 = 0
--          |otherwise = 1


-- Pode usar True ou otherwise na ultima linha dos guardas
abs n |n >= 0 = n
      |n < 0 = -n
       
sayMe :: Int -> String  
sayMe 1 = "Um!"  
sayMe 2 = "Dois!"  
sayMe 3 = "Tres!"  
sayMe 4 = "Quatro!"  
sayMe 5 = "Cinco!"  
sayMe x = "Nao esta entre 1 e 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)



-- somatoria :: Int-> [Int]
-- somatoria s = sum [0..s]

-- media :: [Double,Double,Double]->Double
-- media [x, y, z] = (x+y+z)/3


-- Exercicio1
media3 :: Double->Double->Double->Double
media3 x y z = (x+y+z)/3


-- Exercicio2
maior2_1 :: Double->Double->Double
maior2_1 x y = if x>y then x else y

-- Exercicio3
maior2 :: Double->Double->Double
maior2 x y 
    |x>y = x
    |otherwise = y

-- Exercicio4
maior3a :: Double->Double->Double->Double
maior3a x y z = if x>=y && x>=z then x else if y>=x && y>=z then y else z 

-- Exercicio5
maior3g :: Double->Double->Double->Double
maior3g x y z
    |x>=y && x>=z = x
    |y>=x && y>=z = y
    |otherwise = z    

-- Exercicio6
maior3c2 :: Double->Double->Double->Double
maior3c2 x y z
    |(maior2 x y == x) && (maior2 x z == x) = x
    |(maior2 y x == y) && (maior2 y z == y) = y
    |otherwise = z   

--EXERCICIO 6.1
-- maior6 :: Double -> Double -> Double -> Double
--     maior6 x y z = maior2 x (maior2 y z)    


-- Exercicio7 x² -5x +6 = 0 -> 2 e 3
--  delta = sqrt(b² - 4ac)
disc :: Float->Float->Float->Float
disc a b c = 
  let p1 = b*b;
      p2 = 4*a*c 
  in p1 - p2   

resolve :: Float->Float->Float->String
resolve a b c 
  | sqrt (disc a b c) > 0 = "Duas raizes reais e DISTINTAS: Raiz1:"  ++  show r1 ++ " Raiz2:"++ show r2
  | sqrt (disc a b c) == 0 = "Duas raizes reais e IGUAIS  Raiz1:" ++ show r1 ++ " Raiz2:"++ show r2
  | otherwise = error "NAO POSSUI RAIZES REAIS!" 
  where
    r1 = (-b + sqrt (disc a b c) ) / 2*a
    r2 = (-b - sqrt (disc a b c) ) / 2*a


-- EXERCICIO 8, essa funcao so retorna falso quando os 3 sao iguais
misterio m n p = not (m==n&&n==p)
    
-- let pares = [(1,8),(2,5),(0,1),(4,4),(3,2)] h = sum.map (\(x,y) -> x*y-1).filter (\(x, _) -> even x) in h pares


mult= \x -> \y -> \z -> x*y*z



intercala :: a -> [a] -> [[a]]
intercala n [] = [[n]]
intercala n (x:xs) = [n:x:xs] ++ map (x:)(intercala n xs)



elemento::Eq a => a-> [a] -> Bool
elemento _ [] = False
elemento n (x:xs)
  |n==x = True
  |otherwise = elemento n xs


-- a funcao mp, aplica uma funcao (que aplica em dois termos) tendo como parametro duas listas
mp f [] ys = []
mp f xs [] = []
mp f (x:xs) (y:ys) = f x y : mp f xs ys



-- DEFINIR O TIPO COMPLEXO
-- data Complexo = {a::Real, b::Imag} deriving (Show)
data Complexo = Complexo Double Double 
instance Num Complexo where
  (-) (Complexo a b) (Complexo c d) = (Complexo (a-c) (b-d))
  (+) (Complexo a b) (Complexo c d) = (Complexo (a+c) (b+d))

instance Show Complexo where
  show (Complexo a b) = if  (b<0) then show a ++ show b ++ "i" else show a ++ "+" ++ show b ++ "i"

parteReal :: Complexo -> Double
parteReal (Complexo a _) = a

parteImaginaria :: Complexo -> Double
parteImaginaria (Complexo _ b) = b


c1::Complexo
c1 = Complexo 4 3

c2::Complexo
c2 = Complexo 6 7


data Cor = Azul | Verde | Amarelo | Vermelho deriving (Show)
instance Eq Cor where
  Azul == Azul= True
  Verde == Verde= True
  Amarelo == Amarelo= True
  Vermelho == Vermelho= True
  _ == _= False

data PontoCor = Pt Double Double Cor deriving (Show)
instance Eq PontoCor where
  (==) (Pt x1 y1 c1) (Pt x2 y2 c2) = (x1==x2) && (y1==y2) && (c1==c2)
  -- (Pt x1 y1 c1) == (Pt x2 y2 c2) = (x1==x2) && (y1==y2) && (c1==c2)

-- data Complexo2 = Complexo2 {real::Double, imaginaria::Double} deriving (Show)
-- instance Num Complexo2 where
--   (-) (Complexo a b) (Complexo c d) = (Complexo (a-c) (b-d))
--   (+) (Complexo a b) (Complexo c d) = (Complexo (a+c) (b+d))

-- c3::Complexo2
-- c3 = Complexo 4 3

-- c4::Complexo2
-- c4 = Complexo 6 7



-- Nova Classe Person que deriva o Show e é uma instancia da classe EQ
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   

instance Eq Person where
  (==) (Person a b c d e f) (Person g h i j k l) = (a==g) && (b==h)  

p1::Person
p1 = Person "Fabricio" "Ziliotti" 20 32 "foda" "fodao"

p2::Person
p2 = Person "murilo" "Guerreiro" 32 32 "da" "dsa"

p3::Person
p3 = Person "murilo" "Guerreiro" 3341 2121 "da" "ddasdassa"




data Forma = Circulo Float Float Float | Retangulo Float Float Float Float deriving (Show)
area:: Forma -> Float
area (Circulo _ _ r) = pi * r ^ 2
area (Retangulo a b _ _) = a*b


--TIPOS SINONIMOS
type Teste = Int
teste1::Teste
teste1 = 4

type Teste2 = Double
teste2::Teste2
teste2 = 4.2


-- TIPOS RECURSIVOS
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr deriving (Show)
eval::Expr->Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


-- listas de qualquer tipo de componente, nao somente inteiro como em:
-- data Lista = Nil | Cons Int List
data Lista a = Nil | Cons a (Lista a)



