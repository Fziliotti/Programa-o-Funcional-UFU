import Prelude hiding (replicate)

type Nome = String
type Preco = Int
type CodBar = Int
type BaseDeDados = [(CodBar,Nome,Preco)]
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome, Preco)]
listaDeProdutos :: BaseDeDados

-- lista de produtos é uma base de dados (lista de tuplas)
listaDeProdutos = [(1234, "Oleo DoBom, 1l", 195),
                   (4756, "Chocolate Cazzeiro, 250g", 180),
                   (3216, "Arroz DoBom, 5Kg", 213),
                   (5823, "Balas Pedregulho, 1Kg", 379),
                   (4719, "Queijo Mineirim, 1Kg", 449),
                   (6832, "Iogurte Maravilha, 1Kg", 499),
                   (1112, "Rapadura QuebraDente, 1Kg", 80),
                   (1111, "Sal Donorte, 1Kg", 221),
                   (1113, "Cafe  DoBom, 1Kg", 285),
                   (1115, "Biscoito Bibi, 1Kg", 80),
                   (3814, "Sorvete QGelo, 1l", 695)]


formatacentavos :: Preco -> String
-- mostra na tela o valor 195 como 1.95,em que 1 é a divisao inteira de x por 100 e 95 é o resto da divisao de x por 100
formatacentavos x | x >=10 =  show (x `div` 100) ++ "." ++ show (x `mod` 100)
                  |otherwise = show (x `div` 100) ++ ".0" ++ show (x `mod` 100)

replicate :: Int -> Char -> String
replicate 1 c = "."
replicate n c = "." ++ (replicate (n-1) c)

-- Formatação da linha colocando o numero de pontos necessarios (até 30 carac por linha)
formatalinha :: (Nome, Preco) -> String
formatalinha (a, b) =  a ++ replicate (30 - (length a + length (formatacentavos b))) '.'  ++ (formatacentavos b)

formataLinhas :: [(Nome,Preco)] -> IO ()
formataLinhas [] = putStrLn("")
formataLinhas ((x,xs):xy) = do
              putStrLn(formatalinha(x,xs))
              formataLinhas xy

geraTotal :: [(Nome,Preco)] -> Preco
geraTotal [] = 0
geraTotal ((x,xs):xy) = xs+geraTotal xy

formataTotal :: Preco -> IO()
formataTotal x = putStrLn("Total" ++ replicate (29 - length (formatacentavos x)) '.' ++ "$" ++ (formatacentavos x))

formataRecibo :: [(Nome,Preco)] -> IO ()
formataRecibo [] = putStrLn("")
formataRecibo ((x,xs):xy) = do
              putStrLn("Supermercado QLegal")
              formataLinhas ((x,xs):xy)
              formataTotal (geraTotal ((x,xs):xy))


acha :: BaseDeDados -> CodBar ->(Nome,Preco)
acha [] n = ("item desconhecido", 0)
acha ((a,b,c):xs) n | a==n = (b,c)
                    |otherwise = acha xs n

achaItem :: CodBar -> (Nome,Preco)
achaItem cod = acha listaDeProdutos cod

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) = (achaItem x):fazRecibo xs

geraRecibo :: ListaDeCodigos -> IO ()
geraRecibo (x:xs) = formataRecibo(fazRecibo(x:xs))
