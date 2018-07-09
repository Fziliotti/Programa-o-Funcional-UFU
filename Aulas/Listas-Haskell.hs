
type Seq_Caract = String
type Nomes = (Seq_Caract, Seq_Caract, Seq_Caract,Seq_Caract)

f_nomes_est :: Nomes
f_nomes_est = ("Inverno", "Outono", "Primavera", "Verao")


type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo
pessoa rg
    |rg == 1 = ("Fabricio",12,'m')
    |rg == 2 = ("Marcos",12,'m')
    |rg == 3 = ("Guilherme",24,'m')
    |otherwise = ("Nao ha ninguem",0,'x')


-- LISTAS
criapalindromo x = x ++ reverse x
geraPalindromos n = [1..n] ++ reverse [1..n]

primeiros :: Int->[Int]->[Int]
primeiros 0 _ = []
primeiros _ [] =[]
primeiros n (a:as) = a: primeiros (n-1) as

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x


tamanhoCauda :: [Int] -> Int -> Int
tamanhoCauda [] aux = aux
tamanhoCauda (a:b) aux = tamanhoCauda (b) (aux+1) 


-- concatena :: [Int] -> [Int] -> [Int]
-- concatena [] l = l
-- concatena (x:xs) l = x:concatena (xs) l

-- Dobralista por compreensao, temos que todos os elementos de xs sao multiplicados por x
dobraLista :: [Int] -> [Int]
dobraLista xs = [2*x | x<-xs]

dobraLista2 :: [Int] -> [Int]
dobraLista2 [] = []
dobraLista2 (a:x) = 2*a : dobraLista x

incrementar :: (Num a) => [a] -> [a]
incrementar xs = [ x+1 | x <- xs]




-- map :: (a -> b) -> [a] -> [b]
-- map f xs = [ f x | x <- xs ]

-- map f [] = []
-- map f (x:xs) = f x : map f xs

-- APLICA A FUNCAO DOBRO A TODOS OS ELEMENTOS DA LISTA
dobro x = 2*x
aplica_a_todos:: (Int->Int, [Int])->[Int]
aplica_a_todos(f,l) = [f x | x<-l]

aplica_todos3:: (Int->Int) -> [Int] -> [Int]
aplica_todos3 f [] = []
aplica_todos3 f (x:xs) = f x: aplica_todos3 f xs




comprimentoLista :: [a] -> Int
comprimentoLista [] = 0
comprimentoLista (x:xs) = 1 + comprimentoLista xs


primeiroElemento (x:_)  = x

ultimoElemento (x:[])  = x
ultimoElemento (x:xs) = ultimoElemento xs

mylast [x] = x
mylast (x:xs) = mylast xs
  
fatores n = [i | i<-[1..n], n `mod` i == 0]

maior [x] = x
maior (x:y:resto)
  | x > y = maior (x: resto)
  | otherwise = maior (y: resto)


pertence :: Eq a => a -> [a] -> Bool  
pertence _ [] = False
pertence e (x:xs)
  |e == x = True
  |otherwise = pertence e xs

uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [b | b <- bs, not (pertence b as)]

concatena::[a]->[a]->[a]
concatena [] y = y
concatena (x:xs) y = x: concatena xs y


uniaoR:: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xz) l = if pertence x l then uniaoR xz l
else x: uniaoR xz l

n_esimo :: Int -> [a] -> a
n_esimo 1 (x:xs) = x
n_esimo n (x:xs) = n_esimo (n-1) xs


somaR :: Int -> Int
somaR 0 = 0
somaR x = somaR (x-1) + x

-- aplica_todos2 (f,[]) = []
-- aplica_todos2 (f, (x:y)) = [f x]: aplica_todos2(f,y)

-- aplicaFuncao [Int] -> [Int] -> [Int]
-- aplicaFuncao xs f = [f (x) | x->(xs)]