-- verifica se um número é perfeito
-- número perfeito é o número que é igual a soma de todos os
-- seus divisores excluindo ele próprio

-- função que retorna todos os divisores de um número
-- recebe o número e retorna uma lista dos divisores desse número
obter_fatores :: Int -> [Int]
obter_fatores num = [ x | x <- [1 .. num-1], ((mod num x) == 0)]


-- verifica se o número é perfeito
-- recebe o numero, retorna True se for perfeito e False caso contrário
eh_perfeito :: Int -> Bool
eh_perfeito num
    | ((sum (obter_fatores num)) == num) = True
    | otherwise = False


-- retorna uma lista com todos os números perfeitos até "n"
obter_perfeitos :: Int -> [Int]
obter_perfeitos n = [x | x <- [1 .. n], ((eh_perfeito x) == True)]



-- ---------------------------------------------------------------------------
-- FUNCOES DO TIPO INT (div, mod, abs, negate)
mini :: Int -> Int -> Int -- função que mostra o menor valor entre dois inteiros
mini a b
    | a <= b = a
    | otherwise = b



-- script do meu primeiro operador
(&&&) :: Int -> Int -> Int
a &&& b
    | a < b = a
    | otherwise = b    



verIdade :: (String, Int) -> Int -- Função que se passa uma tupla
verIdade (a,b) = b -- (Nome, Idade), e devolve a idade


fatorial :: Int -> Int
fatorial 0 = 1 
fatorial n = n * fatorial (n-1)


somaLista :: [Int] -> Int
somaLista [] = 0 
somaLista (a:x) = a + somaLista x --PARECIDO COM PROLOG, :x é a cauda da lista


dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (a:x) = 2*a : dobraLista x

length1 :: [t] -> Int
length1 [] = 0
length1 (a:x) = 1 + length1 x


insere :: Int -> [Int] -> [Int]
insere e (a:x)
    | e <=a = e:(a:x)
    | otherwise = a : insere e x
    
ordenacao :: [Int] -> [Int]
ordenacao [] = []
ordenacao (a:x) = insere a (ordenacao x)

somaPares :: [(Int, Int)] -> [Int]
somaPares lista = [ a+b | (a,b) <- lista]

iguais :: Int -> Int -> Bool
iguais x y |x==y = True
iguais x y = False

-- exercicio DA P2
collatz::Int -> [Int]
collatz 1 = [1]
collatz x
  |even x = (x):collatz xs
  |otherwise = (x):collatz xs'
  where xs = (x `div` 2)
        xs' = (x*3+1)

