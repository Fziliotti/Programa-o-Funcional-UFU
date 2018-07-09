
ask :: String -> IO String
ask question = do 
	putStr question
	getLine


main :: IO ()
main = do 
	nome <- ask "Qual e o seu nome? "
	matr <- ask "Qual e o seu numero de matrıcula? "
	putStrLn ("Bem vindo "++ nome ++ "!")
	putStrLn ("Seu numero de matrıcula eh "++ matr)
