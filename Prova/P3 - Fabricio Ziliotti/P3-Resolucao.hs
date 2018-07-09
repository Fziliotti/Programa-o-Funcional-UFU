
import System.IO

type Nome = String
type Preco = Float
type Cod = Int
type Quant = Int


data Produto x = Prdt Nome Preco Cod Quant deriving (Show, Read)

escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo um novo produto no arquivo..."
        h <- openFile f WriteMode
        hFlush h
        escreveDadosDeArquivo 5 h
        hClose h

escreveDadosDeArquivo :: Int -> Handle -> IO ()
escreveDadosDeArquivo n h
  | n == 0 = return ()
  | otherwise =
       do
          putStr "Digite o nome do produto: "
          nome <- getLine
          putStr "Digite o preco do produto: "
          preco <- readLn
          putStr "Digite o codigo de barras do produto: "
          cod <- readLn
          putStr "Digite a quantidade em estoque: "
          quant <- readLn
          putStrLn "-----------------------------"
          let prod = Prdt nome (preco::Preco) cod quant
          hPutStrLn h (show prod)
          escreveDadosDeArquivo (n-1) h

leDados :: String -> IO ()
leDados f = 
     do
        h <- openFile f ReadMode
        hFlush stdout
        leDadosDeArquivo h 0
        hClose h

leDadosDeArquivo :: Handle -> Int -> IO ()
leDadosDeArquivo h c =
   do
     x <- hIsEOF h
     if x
     then do
     	putStrLn ("Quantidade de produtos inexistentes em estoque: " ++ (show c))
     	return ()
     else
       do
        y <- hGetLine h
        let (Prdt nome preco cod quant) = read y
        if (quant==0) then do
         leDadosDeArquivo h (c+1)
         else leDadosDeArquivo h c
