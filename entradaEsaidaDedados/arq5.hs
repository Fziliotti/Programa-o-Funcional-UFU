module Main (main) where


mostraLista :: Show a => [a] ->IO ()
mostraLista xs
  |null xs = return()
  |otherwise = do 
	putStrLn (show (head xs))
	mostraLista (tail xs)


main :: IO()
main = mostraLista [0 , 2..30]

mostraPares:: IO()
mostraPares = do print ([0,2..30])