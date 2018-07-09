module Main where

import System.IO (stdout, hFlush, hSetBuffering, BufferMode(NoBuffering)) 

ask :: String -> IO String
ask question = do putStr question
                  getLine

main :: IO ()
main = do nome <- ask "Qual eh o seu nome? "
          matr <- ask "Qual eh o seu numero de matrıcula? "
          putStrLn ("Bem vindo "++ nome ++ "!")
          putStrLn ("Seu numero de matrıcula eh "++ matr)


main2 :: IO()
main2 = do
    nome <- ask "Qual seu nome? "
    telefone <- ask "Qual seu telefone? "
    putStrLn ("Seu nome eh "++ nome ++ " e seu telefone eh "++ telefone)



-- SOMAR INTEIROS

leInt :: IO(Int)
leInt = do
	putStr "Digite um valor inteiro: "
	readLn

leInt2 :: IO(Int)
leInt2 = do 
	putStr "Digite um valor inteiro: "
	n <- getLine
	return (read n)	

main3 :: IO()
main3 = do 
	n1 <- leInt2
	n2 <- leInt2
	putStr "A soma eh: "
	print (n1+n2)


-- EXERCICIO

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


main5 :: IO()
main5 = do 
	putStr "Digite o primeiro numero: "
	n1 <- readLn
	putStr "Digite o Segundo numero: "
	n2 <- readLn
	putStr "Digite o Terceiro numero: "
	n3 <- readLn
	putStr "O produto eh: "
	print (n1 * n2 * n3)


main6 :: IO()
main6 = do
	putStr "Digite um caractere: "
	char <- getChar
	putChar char	
	putStr "\n"

main7 :: IO()
main7 = do
	putStr "Digite uma string: "
	linha <- getLine
	putStr linha
	putStr "\n"	
	

main8:: IO()
main8 = do 
	--getLine :: IO String
	putStr "Digite o primeiro numero: "
	n1 <- getLine
	putStr "Digite o Segundo numero: "
	n2 <- getLine
	putStr "Digite o Terceiro numero: "
	n3 <- getLine
	putStr "O produto eh: " 
	--print :: Show a => a -> IO ()
	--read é o contrario de show
	--read :: Read a => String -> a
	print (read (n1) * read (n2) * read (n3))



palindromo:: String -> Bool
palindromo x
    |x == reverse x = True
    |otherwise = False

main9:: IO()
main9 = do
	putStr "Digite uma palavra para verificar se eh palindromo: "
	p1 <- getLine
	if (palindromo (p1)) 
		then (putStrLn ("A palavra " ++ p1 ++ " eh palindromo! ")) 
		else (putStrLn ("A palavra " ++ p1 ++ " nao eh palindromo! "))


main10:: IO()
main10 = do
	putStrLn"Digite um número:"
	s1 <- getLine
	putStrLn"Digite outro número:"
	s2 <- getLine
	putStr"Soma dos números digitados:"
	putStrLn (show (read s1+read s2))


main11:: IO()
main11 = do
	putStrLn"Digite um número:"
	s1 <- readLn
	putStrLn"Digite outro número:"
	s2 <- readLn
	putStr"Soma dos números digitados usando print:"
	print (s1 + s2)
	-- putStr"Soma dos números digitados usando putStr:"
	-- putStrLn (show ( s1 +  s2))

-- USANDO O hFLUSH
main12:: IO()
main12 = do
	putStrLn"Digite um número:"
	hFlush stdout
	s1 <- getLine
	putStrLn"Digite outro número:"
	hFlush stdout
	s2 <- getLine
	putStr"Soma dos números digitados:"
	putStrLn (show ((read s1::Float) + (read s2::Float)))


-- USANDO UM  hSetBuffering PRA TENTAR MINIMIZAR O USO DO hFlush
main13:: IO()
main13 = do
	hSetBuffering stdout NoBuffering
	putStr"Digite um número:"
	s1 <- readLn
	putStr"Digite outro número:"
	s2 <- readLn
	putStr"Soma dos números digitados:"
	let soma = (s1::Float) +(s2::Float)
	putStrLn (show soma)


prompt :: Read a => String -> IO a
prompt msg = do {putStr msg; readLn}


main60 :: IO()
main60 = do 
	hSetBuffering stdout NoBuffering
	n1 <- prompt "Digite a primeira nota: "
	n2 <- prompt "Digite a segunda nota: "
	n3 <- prompt  "Digite a terceira nota: "
	let media = (((n1::Double) + (n2::Double) + (n3:: Double))/3)
	if (media <3) then
		putStrLn "Aluno reprovado!"
		else if (media >=3 && media <7) then
			putStrLn "Exame especial!"
			else putStrLn "Aluno aprovado!"

raizes2grau a b c |d>0 = [ (-b+sqrt d)/(2*a), (-b-sqrt d)/(2*a) ]
                  |d==0 = [-b/(2*a) ]
                  |otherwise = [ ]
                  where d = b^2-4*a*c


main70 :: IO()
main70 = do hSetBuffering stdout NoBuffering
           n1 <- prompt "Digite o coeficiente a: "
           n2 <- prompt "Digite o coeficiente b: "
           n3 <- prompt "Digite o coeficiente c: "
           case raizes2grau n1 n2 n3 of
            [r1,r2] -> putStrLn("RaizeS: " ++ show r1 ++ " e " ++ show r2)
            [r] -> putStrLn("Raiz: " ++ show r)
            [] -> putStrLn "Não há raízes reais"			