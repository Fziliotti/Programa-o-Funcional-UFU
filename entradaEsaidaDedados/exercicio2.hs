import Data.Char
import Data.List
import System.IO (stdout, hFlush, hSetBuffering, BufferMode(NoBuffering)) 

------------------------funcao auxiliar---------------------------------------------
ask :: String -> IO String
ask question = do 
	putStr question
	getLine


------------------------EX1---------------------------------------------
verificalinha :: String -> Bool  
verificalinha linha = if ((elem(False).map isLetter)linha == True) then False else True 

invertePalavra :: String -> String
invertePalavra string = reverse string

exercicio1 :: IO()
exercicio1 = do  
	linha <- getLine
	let  v1 = verificalinha linha
	if v1 == True then 
		putStrLn(invertePalavra linha) 
		else putStrLn ("A linha contem caracteres que nao sao alfabeticos!")


------------------------EX2---------------------------------------------
exercicio2 :: IO ()
exercicio2 = do 
	nome <- ask "Qual e o seu nome? "
	sobrenome <- ask "Qual e o seu sobrenome?"
	putStrLn ("Bem vindo "++ nome ++ sobrenome ++ "!")


------------------------EX3---------------------------------------------
upperLinha :: String -> String
upperLinha str = map toUpper str

exercicio3 :: IO()
exercicio3 = do
	linha <- ask "Digite uma linha: \n"
	if null linha 
		then do
			putStrLn ("Programa de leitura finalizado!")
		else do
			putStrLn (upperLinha linha)
			exercicio3

------------------------EX4---------------------------------------------
-- leIntList:: IO()
-- leIntList = do
-- 	num <- ask "Digite um valor: \n"
-- 	let listaNum = adiciona num
-- 	if num == "0"
-- 		then do
-- 			putStrLn (n1)
-- 			putStrLn ("Programa de leitura finalizado!")
-- 		else do
-- 			leIntList


------------------------EX6---------------------------------------------
teste :: IO ()
teste = do 
	putStr "Digite um valor: "
	x <- readLn
	print ([1]++[2,4..x])


---------------------------------------OUTRA LISTA--------------------------------------

leFloat :: IO(Float) 
leFloat = do 
	putStr "Digite um valor float: "
	readLn


main4 :: IO()
main4 = do 
	n1 <- leFloat
	n2 <- leFloat
	n3 <- leFloat
	putStr "O produto eh: "
	print (n1 * n2 * n3)

-- TESTE---------------------------------
testandoInteract:: IO()
testandoInteract = interact (takeWhile isUpper)

prompt:: Read a => String -> IO a
prompt msg = do 
	putStr msg
	readLn

main10 :: IO()
main10 = do hSetBuffering stdout NoBuffering
            putStrLn "-------------------------"
            putStrLn "Opções:"
            putStrLn "-------------------------"
            putStrLn "1. Imposto"
            putStrLn "2. Novo salário"
            putStrLn "3. Classificação"
            putStrLn "-------------------------"
            op <- prompt "Digite a opção desejaqda: "

            if(op == 1) then do
              salario <- prompt "Digite o salário: "
              if((salario::Double) >0 && (salario::Double) <500.00) then 
                putStrLn(show((salario::Double) *0.05))
                else if ((salario::Double) >=500.00 && (salario::Double) <=850.00) then
                  putStrLn(show((salario::Double)*0.15))
                  else putStrLn(show((salario::Double)*0.10))
            else if (op == 2) then do
              salario <- prompt "Digite o salário: "
              if((salario::Double) >1500.00) then
                putStrLn(show((salario::Double)+ 25.00))
                else if ((salario::Double) >=750.00 && (salario::Double) <=1500.00) then
                  putStrLn (show((salario::Double) + 50.00))
                  else if ((salario::Double) >=450.00 && (salario::Double) <750.00) then
                    putStrLn (show((salario::Double)+75.00))
                    else putStrLn (show((salario::Double)+100.00))
            else if(op == 3) then do
              salario <- prompt "Digite o salário: "
              if((salario::Double) >0 && (salario::Double) <=750.00) then
                putStrLn "Mal remunerado!"
                else putStrLn "Bem remunerado!"
            else putStrLn "Opção Inválida"

