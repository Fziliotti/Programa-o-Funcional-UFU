import System.IO
import Data.Char
import Data.List

type Nome = String
type Preco = Int
type CodBarra = Int
type QtdEst = Int
data Produto = Prod Nome Preco CodBarra QtdEst deriving (Show,Read)

--Ao ser usado na main retorna um Numero
promptNUM:: Read a =>String ->IO a
promptNUM msg = do
	putStr msg
	readLn

--Ao ser usada na main retornara uma String
promptTXT msg = do 
	putStr msg
	getLine

--ARQUIVO DE SAIDA QUE É PEDIDO NO EXERCICIO
arquivoSaida::String
arquivoSaida = "produtos.dat"


--Produtos para teste
produto1 :: Produto
produto1 = (Prod "Cadeira" 100 111 10)

produto2 :: Produto
produto2 = (Prod "Teclado" 120 222 20)

produto3 :: Produto
produto3 = (Prod "Apagador" 10 333 30)

produto4 :: Produto
produto4 = (Prod "mouse" 10 333 0)

produto5 :: Produto
produto5 = (Prod "monitor" 10 333 0)

produtosTeste:: [Produto]
produtosTeste = [produto1,produto2,produto3,produto4,produto5]


--Funcao que verifica se dado produto é inexistente no estoque
prodInexistente:: Produto -> Bool
prodInexistente (Prod n p c q) = if (q ==0) then True else False

imprimeProduto:: Produto -> IO()
imprimeProduto (Prod n p c q) = do 
	putStrLn("Nome do produto eh: " ++  n)
	putStrLn("Preco do produto eh: " ++ show p)
	putStrLn("Codigo de barras do produto eh: " ++ show c)
	putStrLn("Quantidade em estoque :" ++ show q)


main:: IO()
main = do 
	nome <- promptTXT("Digite o nome do Produto: ")
	preco <- promptNUM("Digite o Preco do produto: ")
	codbarra <- promptNUM("Digite o codigo de barra do produto: ")
	qtdEst <- promptNUM("Digite a quantidade em estoque do produto: ")
	let p1 = (Prod nome preco codbarra qtdEst)
	
	--Informacoes do produto obtidas com sucesso!
	--Agora deve-se armazenar no arquivo Produtos.dat

	--Verificacao se o produto é existente
	if prodInexistente p1 then do putStrLn("Nao existente em estoque!") else do imprimeProduto p1


--Funcao que verifica se dado produto é 
prodInexistenteAux:: Produto -> Int
prodInexistenteAux (Prod n p c q) = if (q ==0) then 1 else 0


--Funcao que pega uma lista de produtos e retorna o numero de produtos que nao possuem em estoque
qtdProdutosInexistentes:: [Produto] -> Int
qtdProdutosInexistentes produtos = (sum.(map prodInexistenteAux)) produtos





--Insere cada campo do produto no arquivo de saida
-- escreveDados:: IO()
-- escreveDados = do
-- 	h <- openFile "produtos.dat" WriteMode
-- 	hFlush h
-- 	nome <- promptTXT("Digite o nome do Produto: ")
-- 	hPutStrLn h nome
-- 	preco <- promptNUM("Digite o Preco do produto: ")
-- 	hPutStrLn h (read nome)
-- 	codbarra <- promptNUM("Digite o codigo de barra do produto: ")
-- 	hPutStrLn h (read codbarra)
-- 	qtdEst <- promptNUM("Digite a quantidade em estoque do produto: ")
-- 	hPutStrLn h (read qtdEst)
   


--Insere o Prod no arquivo produtos.dat 
-- escreveDados2:: IO()
-- escreveDados2 = do
-- 	h <- openFile "produtos.dat" WriteMode 
-- 	nome <- promptTXT("Digite o nome do Produto: ")
-- 	preco <- promptNUM("Digite o Preco do produto: ")
-- 	codbarra <- promptNUM("Digite o codigo de barra do produto: ")
-- 	qtdEst <- promptNUM("Digite a quantidade em estoque do produto: ")
-- 	let p1 = (Prod nome preco codbarra qtdEst)
-- 	hPutStrLn p1
	



-- FUNCOES DADA PELO EXERCICIO DE MANIPULACAO DE ARQUIVOS

escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo em arquivo ..."
        h <- openFile f WriteMode
        hFlush h


        escreveDadosDeArquivo 5 h
        hClose h

-- escreveDadosDeArquivo :: Int -> Handle -> IO ()
-- escreveDadosDeArquivo n h
--   | n == 0 = return ()
--   | otherwise =
--        do
--        	nome <- promptTXT("Digite o nome do Produto: ")
--        	hPutStrLn h nome
--        	preco <- promptNUM("Digite o Preco do produto: ")
--        	hPutStrLn h preco
--        	codbarra <- promptNUM("Digite o codigo de barra do produto: ")
--        	hPutStrLn h codbarra
--        	qtdEst <- promptNUM("Digite a quantidade em estoque do produto: ")
--        	hPutStrLn h qtdEst
--         escreveDadosDeArquivo (n-1) h

leDados :: String -> IO ()
leDados f = 
     do
        h <- openFile f ReadMode
        hFlush stdout
        leDadosDeArquivo h
        hClose h

leDadosDeArquivo :: Handle -> IO ()
leDadosDeArquivo h =
   do
     x <- hIsEOF h
     if x
     then return ()
     else
       do
        y <- hGetLine h
        putStrLn y