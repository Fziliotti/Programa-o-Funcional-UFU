
-- Questão 1: O tempo de espera total é 3358 minutos.
-- Questão 2: 5

type TempoDeAtend = Int
type TempoQChegou = Int
type TempoPAtend = Int
type Tempo = Int
type TempoDEsp = Int

nroDeFilas = 4

data ClienteQChega = Nao | Sim TempoQChegou TempoPAtend deriving (Show)
data ClienteQSai = Nenhum | Liberado TempoQChegou TempoDEsp TempoPAtend deriving (Show)


type Fila = [Int]

type EstadoDaFila = (Tempo,TempoDeAtend,[ClienteQChega])
type EstadoDoServidor = [EstadoDaFila]

--------EstadoDaFila------------

adicionaCliente :: ClienteQChega -> EstadoDaFila -> EstadoDaFila
adicionaCliente m (tempo,tempDeAtend,ml) = (tempo,tempDeAtend, ml++[m])

processaFila ::EstadoDaFila -> (EstadoDaFila,[ClienteQSai])
processaFila (tempo,tempDeAtend,(Sim a tempNecDAtend:resto))
    | tempDeAtend < tempNecDAtend =(((tempo+1),tempDeAtend+1,(Sim a tempNecDAtend:resto)),[])
    | otherwise =((tempo+1,0,resto),[Liberado a (tempo-tempNecDAtend-a) tempNecDAtend])
processaFila (tempo,tempDeAtend,[]) = ((tempo+1,tempDeAtend,[]),[])

----------Servidor----------

colocaNaFila :: Int -> ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
colocaNaFila n im st = take n st
 ++ [novoEstadoDaFila]
 ++ drop (n+1) st
 where
 novoEstadoDaFila = adicionaCliente im (st!!n)

processaServidor :: EstadoDoServidor -> (EstadoDoServidor, [ClienteQSai])
processaServidor [] = ([],[])
processaServidor (q:qs) = ((nq:nqs), mess ++ messes) where
    (nq,mess) = processaFila q
    (nqs,messes) = processaServidor qs

processaSimulacao :: EstadoDoServidor -> ClienteQChega -> (EstadoDoServidor,[ClienteQSai])
processaSimulacao estServ im = (adicionaNovoObjeto im estServ1,clientQSai) where (estServ1,clientQSai) = processaServidor estServ

adicionaNovoObjeto :: ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
adicionaNovoObjeto Nao estServ = estServ
adicionaNovoObjeto (Sim tempoDeChegada tempoNecAtend) estServ = colocaNaFila (menorFila estServ)(Sim tempoDeChegada tempoNecAtend) estServ

estadoInicialDoServidor :: EstadoDoServidor
estadoInicialDoServidor = copy nroDeFilas filaDeInicio

copy :: Int->a->[a]
copy 0 a = [] 
copy y a = [a]++(copy (y-1) a)

tamanhoDoServidor :: EstadoDoServidor -> Int
tamanhoDoServidor = length

menorFila :: EstadoDoServidor -> Int
menorFila [q] = 0
menorFila (q:qs)
    | tamanhoDaFila (qs!!menor) <= tamanhoDaFila q = menor + 1
    | otherwise = 0 where menor = menorFila qs

estaVazia :: Fila -> Bool
estaVazia [] = True
estaVazia _ = False

enfileira :: Int -> Fila -> Fila
enfileira a x = x ++ [a]

desenfileira :: Fila -> (Int,Fila)
desenfileira x | not (estaVazia x) = (head x,tail x)
               | otherwise = error "erro: a fila esta vazia"

filaDeInicio :: EstadoDaFila
filaDeInicio = (0,0,[])

tamanhoDaFila :: EstadoDaFila -> Int
tamanhoDaFila (tempo,tempoDeAtend,l) = length l

filaVazia :: EstadoDaFila -> Bool
filaVazia (t,s,[]) = True

---------------Aleatoriedade----------------
semente :: Integer
semente = 17489
multiplicador :: Integer
multiplicador = 25173
incremento :: Integer
incremento = 13849
modulo :: Integer
modulo = 65536

proxNumAleat :: Integer -> Integer
proxNumAleat n = ((multiplicador*n) + incremento) `rem` modulo

dist :: Num t => [(t, Float)]
dist = [(1, 0.2), (2, 0.25), (3, 0.25), (4, 0.15), (5, 0.1), (6, 0.05)]

seqAleatoria :: (Integer -> [Integer])
seqAleatoria = iterate proxNumAleat

geraFuncao :: [(t,Float)] -> (Float -> t)
geraFuncao dist = geraFun dist 0.0
geraFun ((ob,p):dist) nUlt aleat
    | nProx >= aleat && aleat > nUlt = ob
    | otherwise = geraFun dist nProx aleat where nProx = p* fromInteger modulo + nUlt



--------------Simulação-----------------

simule :: EstadoDoServidor -> [ClienteQChega] ->  [ClienteQSai]
simule estDoServ (im:messes) = outmesses ++ simule proxEstDoServ messes where (proxEstDoServ,outmesses) = processaSimulacao estDoServ im

entradaDaSimulacao :: [ClienteQChega]
entradaDaSimulacao = zipWith Sim [ 1..] seqDeTempos

seqDeTempos = map (geraFuncao dist.fromInteger) (seqAleatoria semente)

entradaDaSimulacao2 = take 50 entradaDaSimulacao ++ naos
naos = (Nao:naos)

tempoDeEsperaTotal :: ([ClienteQSai] -> Int)
tempoDeEsperaTotal = sum . map tempoDEsp
                     where tempoDEsp (Liberado _ w _) = w
