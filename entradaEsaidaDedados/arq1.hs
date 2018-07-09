main = putStrLn "Hello Word!"
--putStrLn :: String -> IO ()
--Nós podemos ler o tipo de putStrLn assim: putStrLn pega uma string e retorna uma ação de I/O que tem um resultado do tipo () (ou seja: uma tupla vazia, também conhecida como unidade). Uma ação de I/O é alguma coisa que, quando executada, irá realizar uma ação com um efeito colateral (que é usualmente ler do dispositivo de entrada ou imprimir algo na tela) e irá também conter algum tipo de valor de retorno dentro dela. 
--Imprimir uma string no terminal realmente não tem nenhum tipo de valor de retorno significativo, então um valor irrelevante () é usado.

main3::IO()
main3 = do putStrLn "Digite um numero:"
           n1 <- getLine
           putStrLn "Digite outro numero:"
           n2 <- getLine
           putStrLn "Digite mais um numero: "
           n3 <- getLine
           putStr "Soma dos numeros: "
           let soma = (read n1) + (read n2) + (read n3)
           putStrLn (show soma)