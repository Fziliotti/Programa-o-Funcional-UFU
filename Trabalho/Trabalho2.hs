-- SEGUNDO TRABALHO DISCIPLINA PROGRAMAÇÃO FUNCIONAL
-- ALUNO: FABRÍCIO FERNANDES ZILIOTTI 11711BCC002
-- TÍTULO: SIMULAÇÃO DE FILAS
import Data.List
import System.IO

-- TEMPOS
type Tempo = Int
type TempoQChegou = Int
type TempoPAtend = Int
type TempoDEsp = Int
type TempoDeAtend = Int

-- Tipos de Clientes 
data ClienteQChega = Nao | Sim TempoQChegou TempoPAtend deriving(Show, Eq)
data ClienteQSai = Nenhum | Liberado TempoQChegou TempoDEsp TempoPAtend deriving(Show, Eq)

-- Estado da Fila -------------------------------------------------------------
type EstadoDaFila = (Tempo, TempoDeAtend, [ClienteQChega])

--adiciona mais um ClienteQChega no final da fila
adicionaCliente :: ClienteQChega -> EstadoDaFila -> EstadoDaFila
adicionaCliente novoCliente (tempo, tempDeAtend, ml) = (tempo, tempDeAtend, ml ++ [novoCliente])

processaFila :: EstadoDaFila -> (EstadoDaFila, [ClienteQSai])
processaFila (tempo, tempDeAtend, []) = ((tempo + 1, tempDeAtend, []), [])
processaFila (tempo, tempDeAtend, (Sim tempQChegou tempNecDAtend:resto))
       | tempDeAtend < tempNecDAtend = (((tempo + 1), (tempDeAtend + 1), (Sim tempQChegou tempNecDAtend:resto)), [])
       | otherwise = ((tempo+1, 0, resto), [Liberado tempQChegou (tempo - tempNecDAtend - tempQChegou) tempNecDAtend])

-- cria uma fila vazia e com elementos nulos
filaDeInicio :: EstadoDaFila
filaDeInicio = (0, 0, [])

--Verifica a qtd de elementos da fila
tamanhoDaFila :: EstadoDaFila -> Int
tamanhoDaFila (tempo, tempoDeAtend, l) = length l

--Verifica se a lista é vazia ou nao
filaVazia :: EstadoDaFila -> Bool
filaVazia (t , s, q) = (q == [])

-- Estado do Servidor ---------------------------------------------------------
type EstadoDoServidor = [EstadoDaFila] --[(Tempo, TempoDeAtend, [ClienteQChega])]

colocaNaFila :: Int -> ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
colocaNaFila n cChega st = take n st ++ [novoEstadoDaFila] ++ drop (n+1) st
                       where novoEstadoDaFila = adicionaCliente cChega (st!!n)

--Percorre todas as filas e aplica a funcao processaFila
processaServidor :: EstadoDoServidor -> (EstadoDoServidor, [ClienteQSai])
processaServidor [] = ([],[])
processaServidor (q:qs) = ((nq:nqs), mess ++ messes)
                          where (nq,mess) = processaFila q
                                (nqs,messes) = processaServidor qs

processaSimulacao :: EstadoDoServidor -> ClienteQChega -> (EstadoDoServidor,[ClienteQSai])
processaSimulacao estadoAtual cChega = (adicionaNovoObjeto cChega servidorProcessado, clientQSai)
                               where (servidorProcessado, clientQSai) = processaServidor estadoAtual

adicionaNovoObjeto :: ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
adicionaNovoObjeto Nao estServ = estServ
adicionaNovoObjeto (Sim tempoDeChegada tempoNecAtend) estServ = colocaNaFila (menorFila2 estServ) (Sim tempoDeChegada tempoNecAtend) estServ

copy :: Int -> EstadoDaFila ->EstadoDoServidor
copy n x = replicate n x

estadoInicialDoServidor :: Int -> EstadoDoServidor
estadoInicialDoServidor nroDeFilas = copy nroDeFilas filaDeInicio             
-- copy 3 (0, 0, []) vai criar uma lista de n filas vazias dentro do servidor    

tamanhoDoServidor :: EstadoDoServidor -> Int
tamanhoDoServidor x = length x

menorFila :: EstadoDoServidor -> Int
menorFila [q] = 0
menorFila (q:qs)
  | tamanhoDaFila (qs!!menor) <= tamanhoDaFila q = menor + 1
  | otherwise = 0
  where menor = menorFila qs


menorFila2 :: EstadoDoServidor -> Int
menorFila2 x = (head.elemIndices a) b  
           where a = (minimum.map(tamanhoDaFila)) x 
                 b = (map(tamanhoDaFila)) x 
--b vai ser a lista de inteiros
--a vai ser o menor elemento da fila b
--depois eh necesario encontrar o indice desse menor elemento na lista b
--depois como precisamos pegar a primeira fila fazia, pego a cabeça do retorno elemIndices


-- ALEATORIEDADE (Pseudo) EM HASKELL
semente :: Integer
semente = 17489

multiplicador :: Integer
multiplicador = 25173

incremento :: Integer
incremento = 13849

modulo :: Integer
modulo = 65536
-- "Integer" is an arbitrary precision type
-- "Int" is the more common 32 or 64 bit integer. Implementations vary, although it is guaranteed to be at least 30 bits.

--Sao as faixas de cair o numero aleatorio, algumas faixas sao maiores, logo a probabilidade de cair sera maior
dist :: Num t => [(t, Float)]
dist = [(1, 0.2), (2, 0.25), (3, 0.25), (4, 0.15), (5, 0.1), (6, 0.05)]

proxNumAleat :: Integer -> Integer
proxNumAleat n = ((multiplicador * n) + incremento) `rem` modulo

--Esse seqAleatproa gera numeros semialeatorios grandes e cada numero ira ter uma certa "probabilidade especificada em dist", os valores variam de 1 a 6
seqAleatoria :: (Integer -> [Integer])
seqAleatoria = iterate proxNumAleat

geraFuncao :: [(t, Float)] -> (Float -> t)
geraFuncao dist = geraFun dist 0.0
geraFun ((ob,p):dist) nUlt aleat
          | nProx >= aleat && aleat > nUlt = ob
          |otherwise = geraFun dist nProx aleat
                       where nProx = (p * (fromInteger modulo)) + nUlt


-- Simulacao ------------------------------------------------------------------

estadoTeste1::EstadoDoServidor
estadoTeste1 = estadoInicialDoServidor 1

estadoTeste2::EstadoDoServidor
estadoTeste2 = estadoInicialDoServidor 2

estadoTeste3::EstadoDoServidor
estadoTeste3 = estadoInicialDoServidor 3

estadoTeste4::EstadoDoServidor
estadoTeste4 = estadoInicialDoServidor 4

estadoTeste5::EstadoDoServidor
estadoTeste5 = estadoInicialDoServidor 5

estadoTeste6::EstadoDoServidor
estadoTeste6 = estadoInicialDoServidor 6

estadoTeste7::EstadoDoServidor
estadoTeste7 = estadoInicialDoServidor 7

--Vai percorrendo cada ClientQChega da lista passada e aplicando o processaSimulacao
--Retornando a concatenacao dos ClientQSai
simule :: EstadoDoServidor -> [ClienteQChega] -> [ClienteQSai]
simule estDoServ (im:messes) = outmesses ++ simule proxEstDoServ messes
                               where (proxEstDoServ,outmesses) = processaSimulacao estDoServ im



--Essa funcao retorna  uma sequencia aleatoria de tempos
seqDeTempos :: [TempoPAtend]
seqDeTempos = map (geraFuncao dist.fromInteger) (seqAleatoria semente)


--GERA UMA LISTA DE ClienteQ CHEGA  com tempo que chegou incrementando de 1 em 1 e tempoPAtendimento aleatorio
entradaDaSimulacao :: [ClienteQChega]
entradaDaSimulacao = zipWith Sim [1..] seqDeTempos

entradaDaSimulacao2 :: [ClienteQChega]
entradaDaSimulacao2 = take 50 entradaDaSimulacao ++ naos


naos :: [ClienteQChega]
naos = (Nao:naos)

tempoDeEsperaTotal :: ([ClienteQSai] -> Int)
tempoDeEsperaTotal = sum. map tempoDEsp
                     where tempoDEsp (Liberado _ w _) = w




--Exercicio1 RESPOSTA 3358 minutos
exercicio1 :: IO()
exercicio1 = do 
  let a = simule estadoTeste1 entradaDaSimulacao2
  let b = take 50 a
  putStrLn(" ")
  putStrLn("A Lista de clientes que Sairam da fila: ")
  putStrLn(show b)
  let c = tempoDeEsperaTotal b
  putStrLn(" ")
  putStrLn("O tempo de espera total desses clientes em uma unica fila seria: ")
  putStrLn(show c)


--Exercicio2 RESPOSTA: 5 FILAS SAO NECESSARIAS PARA ZERAR O TEMPO
exercicio2 :: IO()  
exercicio2 = do 
  let a = simule estadoTeste5 entradaDaSimulacao2
  let b = take 50 a
  let c = tempoDeEsperaTotal b
  putStrLn(" ")
  putStrLn("O tempo de espera total desses clientes com 5 filas sera: ")
  putStrLn(show c)


-- EXERCICIO-3a RESPOSTA O mesmo do primeiro exercicio, ja que existe uma unica fila
-- EXERCICIO-3b RESPOSTA: 7 FILAS SAO NECESSARIAS PARA ZERAR O TEMPO


----TENTATIVA DO ROUND ROBIN
----EXERCICIOS DA LISTA ------------------------------------------------------------- 
----Modificacoes do Round-Robin

-- colocaNaFilaRound:: Int -> ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
-- colocaNaFilaRound n im st = [novoEstadoDaFila] ++ take ((tamanhoDoServidor st)-1) st where novoEstadoDaFila = adicionaCliente im (st!!((tamanhoDoServidor st)-1))

-- processaSimulacaoRound :: EstadoDoServidor -> ClienteQChega -> (EstadoDoServidor,[ClienteQSai])
-- processaSimulacaoRound estadoAtual cChega = (adicionaNovoObjetoRound cChega servidorProcessado, clientQSai)
--                                where (servidorProcessado, clientQSai) = processaServidor estadoAtual                              
 
-- adicionaNovoObjetoRound :: ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
-- adicionaNovoObjetoRound Nao estServ = estServ
-- adicionaNovoObjetoRound (Sim tempoDeChegada tempoNecAtend) estServ = colocaNaFilaRound 0 (Sim tempoDeChegada tempoNecAtend) estServ

-- simuleRound :: EstadoDoServidor -> [ClienteQChega] -> [ClienteQSai]
-- simuleRound estDoServ (im:messes) = outmesses ++ simuleRound proxEstDoServ messes
--                                where (proxEstDoServ,outmesses) = processaSimulacaoRound estDoServ im                               


-- EXERCICIO RESPOSTA O mesmo do primeiro exercicio, ja que existe uma unica fila
-- exercicio3a :: IO()
-- exercicio3a = do 
--   let a = simuleRound estadoTeste1 entradaDaSimulacao2
--   let b = take 50 a
--   putStrLn(" ")
--   putStrLn("A Lista de clientes que Sairam da fila: ")
--   putStrLn(show b)
--   let c = tempoDeEsperaTotal b
--   putStrLn(" ")
--   putStrLn("O tempo de espera total desses clientes em uma unica fila seria: ")
--   putStrLn(show c)


-- --Exercicio2 RESPOSTA: 7 FILAS SAO NECESSARIAS PARA ZERAR O TEMPO
-- exercicio3b :: IO()  
-- exercicio3b = do 
--   let a = simuleRound estadoTeste7 entradaDaSimulacao2
--   let b = take 50 a
--   let c = tempoDeEsperaTotal b
--   putStrLn(" ")
--   putStrLn("O tempo de espera total desses clientes com 7 filas sera: ")
--   putStrLn(show c)

