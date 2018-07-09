
module Main (main) where
import System.Random (randomRIO)


main :: IO()
main = do
  putStrLn "Chute um numero que cairá no dado:"
  numero <- getLine  
  putStrLn ("Voce acha que será o numero " ++ numero)
  x <- randomRIO (1,6::Int)
  putStrLn ("O numero que caiu no dado foi o " ++ show x)
  if (x == read numero) then putStrLn("Voce acertou o numero!") else putStrLn("Você esta sem sorte hoje!")



main2 :: IO()
main2 = do
  putStrLn "Chute um numero que cairá no dado:"
  numero <- getLine  
  putStrLn ("Voce acha que será o numero " ++ numero)
  x <- randomRIO (1,6::Int)
  putStrLn ("O numero que caiu no dado foi o " ++ show x)
  let num1 = read numero
  if (num1 ==  x) then putStrLn("Voce acertou o numero!") else putStrLn("Você esta sem sorte hoje!")


