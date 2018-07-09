
-- ESSA FOI UMA REVISAO PARA A PRIMEIRA PROVA

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
  (Complexo a b) + (Complexo c d) = (Complexo (a+c) (b+d))
  (Complexo a b) - (Complexo c d) = (Complexo (a-c) (b-d))
  -- (-) (Complexo a b) (Complexo c d) = (Complexo (a-c) (b-d))
  -- (+) (Complexo a b) (Complexo c d) = (Complexo (a+c) (b+d))

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


sum' :: (Num a) => [a]->a
sum' = foldl (+) 0


-- COMPOSICAO DE FUNCOES

-- f x = x^2
-- f2 x = f (f x)
-- f = \x

soma ::Int ->Int ->Int 
soma a b = a+b

soma' :: Int -> (Int ->Int )
soma' x = terminaSoma
  where terminaSoma y = x+y

soma'' :: Int -> (Int ->Int )
soma'' x = \y -> x+y
-- soma'' = \x -> (\y -> x+y)

deriv :: Fractional a => (a -> a) -> a -> a -> a
deriv f dx = \x -> (f(x + dx) - f(x)) / dx



data Talvez a = Valor a | Nada deriving (Show)

divisaoSegura :: Float -> Float -> Talvez Float
divisaoSegura x y = if y == 0 then Nada else Valor (x/y)


addPares :: [(Int,Int)] -> [Int]
addPares lista = [ m+n | (m,n) <- lista , m<n ]


addPares' :: [(Int,Int)] -> [Int]
addPares' lista = map (\(m,n) -> m+n) lista




data NomeP = Nome String deriving (Show)
data SobrenomeP = SobreNome String deriving (Show)
type NomeCompleto = (NomeP,SobrenomeP)

instance Eq NomeP where
  (==) (Nome a) (Nome b) = a==b

instance Eq SobrenomeP where
  (==) (SobreNome a) (SobreNome b) = a==b
  
compara:: NomeCompleto -> NomeCompleto -> Bool
compara (a,b) (c,d)
  |(a==c) && (b==d)= True
  |otherwise = False 

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

eval::Expr->Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


data Lista a = Vazio | Cons a (Lista a)
-- [1,2,3] => Cons 1(Cons 2 (Cons 3 Vazio))

comprimento::Lista a -> Int
comprimento Vazio = 0
comprimento (Cons x xs)  = 1 + comprimento xs


