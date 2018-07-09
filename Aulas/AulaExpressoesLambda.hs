--Dizemos que um tipo de primeira classe é um tipo para o qual não há
--restrições sobre como os seus valores podem ser usados 
--EX: numeros,caracteres,Tuplas,listas,Funcoes

-- Valores de varios tipos podem ser escritos literalmente sem a necessidade de nomear: True 'G' 456 2.65 "teste" [1,5,6] (1,2)..

-- f = \x -> 3*x   --NOMEANDO A FUNCAO
-- f x = 3*x é a mesma coisa que a de cima, mas é mais sucinto

-- Exemplos de aplicações de função usando expressões lambda no ghci:
-- (\x -> 2*x+1) 8

f = \x -> 2*x+1
somaPar = \(x,y) -> x+y
fatorial = \n -> product [1..n]

-- MESMA COISA!
-- f x = 2*x+1
-- somaPar(x,y) = x+y
-- fatorial n = product [1..n]

soma' :: Int -> (Int ->Int)
soma' x = terminaSoma  
  where terminaSoma y = x+y

soma'' :: Int -> (Int ->Int )
soma'' x = \y -> x+y

soma''' :: Int -> (Int ->Int )
soma''' = \x -> (\y -> x+y)

-- Portanto a função soma pode ser considerada como uma função que recebe
-- um argumento e resulta em outra função que, por sua vez, recebe outro
-- argumento e resulta na soma dos dois argumentos.

let somaCinco = soma 5 in somaCinco 8 
-- ~ 13 
map (soma2) [1,8,0,19,5] 
-- ~ [3,10,2,21,7]
(soma2.length)"entendeu?" 
-- ~ 11


-- Funções que recebem os seus argumentos um por vez são chamadas de
-- funções curried, celebrando o trabalho de Haskell Curry no estudo de tais
-- funções.


soma x y = x+y
-- pode ser entendida como
soma = \x -> (\y -> x+y)

-- isto é, soma é uma função que recebe um argumento x e resulta em
-- uma função que por sua vez recebe um argumento y e resulta em x+y.


-- (’c’:)"s"  -> cs , coloca o c na cabeça

-- Determine o valor da exprecao
let pares = [(1,8),(2,5),(0,1),(4,4),(3,2)]
h = sum.map (\(x,y) -> x*y-1).filter (\(x, _) -> even x)
in h pares


-- mult x y z = x*y*z
-- redefina a funcao acima usando expressoes lambda


--Reduza as expressões avaliando-as e escreva o
--código correspondente em Haskell.
--(λ xy.x-y) (λz.z/2)
-- bloco principal e funcao declarada


