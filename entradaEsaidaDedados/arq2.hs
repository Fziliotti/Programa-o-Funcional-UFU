

main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- QUANDO FAZEMOS name <- getLine estamos converrtendo 
--Seu programa ser somente uma ação de I/O parece um tipo de limitação. Por isso nós podemos usar a sintaxe do para unir várias ações de I/O em uma só

--Cada um desses passos é uma ação de I/O. Colocando-as juntas com a sintaxe do, nós as transformamos em uma única ação de I/O. A ação que nós obtivemos tem um tipo IO (), porque esse é o tipo da última ação de I/O dentro dela.