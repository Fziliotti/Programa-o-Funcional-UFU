-- type File = String
-- COM ESSAS TRES FUNCOES DA PARA MANIPULAR ARQUIVOS
-- writeFile :: File -> String -> IO()
-- appendFile :: File -> String -> IO()
-- readFile :: File -> IO String
-- import System.IO
import System.IO
escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo em arquivo ..."
        h <- openFile f WriteMode
        hFlush h
        escreveDadosDeArquivo 5 h
        hClose h

escreveDadosDeArquivo :: Int -> Handle -> IO ()
escreveDadosDeArquivo n h
  | n == 0 = return ()
  | otherwise =
       do
          putStr "Digite um valor:"
          l <- getLine
          hPutStrLn h l
          escreveDadosDeArquivo (n-1) h

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
        leDadosDeArquivo h