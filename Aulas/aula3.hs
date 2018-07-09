
import Prelude hiding ((||), (&&))
-- -- CASAMENTO DE FUNCOES

-- case 3-2+1 of
-- 0 -> "zero"
-- 1 -> "um"
-- 2 -> "dois"
-- 3 -> "tres"

-- case 12>10 of
-- 	True -> "MAIOR"
--     False -> "MENOR"

-- EXERCICIO 1

(||) ::Bool->Bool->Bool
_ || True = True
edTrue || _ = True 
False || False = False

(&&) :: Bool->Bool->Bool
x && y = if x==y then x else False

(&&) :: Bool->Bool->Bool
x && y = if x==True then y else False


-- Casamento de padraos

case 3-2+1 of
	0-> "zero!"
	1-> "um!"
	2-> "dois!"
	3-> "tres!"

