import Data.Char  
  
main = do  
    putStrLn "Qual seu nome?"  
    primeiroNome <- getLine  
    putStrLn "Qual seu ultimo nome?"  
    ultimoNome <- getLine  

    let upperPrimeiroNome = map toUpper primeiroNome  
    let upperUltimoNome = map toUpper ultimoNome  
    --podemos aplicar o map na ultimo nome porque esta é "convertida em string"
    putStrLn $ "Ola " ++ upperPrimeiroNome ++ " " ++ upperUltimoNome ++ ", beleza pura?"  


    -- nameTag = "Hello, my name is " ++ getLine  NAO FUNCIONA POIS ++ precisa de duas listas como parametro e getLine tem tipo IO String