-- POLIMORFISSMO PARAMETRICO E POLIMORFISMO AD HOC (SOBRECARGA)
-- (+) : : a -> a -> a
-- (==) : : a -> a ->Bool
-- As definições não são coerentes, pois são
-- tipos demasiadamente genéricos e fariam
-- com que fossem aceitas expressões
-- como:
-- ’a’+’b’
-- True+False
-- "está"+"errado"
-- div==mod


-- (+) :: Num a => a -> a -> a


-- CLASSES E INSTANCIAS DAS CLASSES

-- Uma classe estabelece um conjunto
-- de assinaturas de funções (os
-- métodos da classe).
-- Deve-se definir os métodos de
-- uma classe para cada um dos
-- tipos que são instâncias desta
-- classe.

class Num a where
	(+) :: a -> a ->a
	(*) :: a -> a ->a


instance Num Int where
	(+) = primPlusInt
	(*) = primMulInt

instance Num Float where
	(+) = primPlusFloat
	(*) = primMulFloat