-- UMA DADA EXPRESSAO VAI SER CALCULADA, QUANDO DE FATO ELA FOR NECESSARIA PARA O CALCULO QUE É PEDIDO, DIFERENTEMENTE DA LINGUAGEM C QUE JA CALCULA ISSO ANTES
-- FUN (1+2) (3+4)
-- É IGUAL A (1+2) + (3+4)
-- EM GERAL AS EXPRESSOES ARITMETICAS SAO ARMAZENADAS NA MEMORIA, COM UM UNICO PONTEIRO APONTANDO PARA ELA

-- A AVALIACAO PREGUIÇOSA AJUDA A TRABALHAR COM LISTAS INFINITAS,
-- DE MODO QUE AS OPERACOES COM ESSAS LISTAS NAO PRECISEM CALCULAR AS LISTAS INFINITAS, dependendo do contexto como somar as duas cabeças de duas listas passadas

f :: Int->Int->Int->Int
f x y z
  |x>=y && x>=z = x
  |y>= x && y>=z = y
  |otherwise = z

-- f (9+2) (3*4) 12
-- no caso acima, caso caia no segundo guarda, o valor de x y e z nao precisa ser calculado novamente


