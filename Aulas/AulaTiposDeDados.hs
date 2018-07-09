-- Tipos algebricos de dados
-- definindo tipo algebrico
data ExNovoTipo = Constr1 Tipo11 Tipo12
| Constr2 Tipo21
| Constr3 Tipo31 Tipo32 Tipo33
| Constr4



 -- Tipos enumeraveis,deve-se definir os valores dos tipos  que devem estar relacionados com os tipos algebricos de dados
 -- essa sequencia de valores sao chamados de construtores
 -- devem necessariamente começar com letra maiuscula

 data Dia = Dom | Seg | Ter | Qua | Qui | Sex | Sab
 data Est = Primavera | Outono | Inverno | Verao

 finalDeSemana Sex = True
 finalDeSemana Sab = True
 finalDeSemana _ = False


-- ESSE DERIVING SERVE PARA DEFINIR AS OPERACOES DO TIPO ENUMERAVEL
data Est = Primavera|Verao|Outono|Inverno
deriving Eq

-- ESSE ind é o nome do construtor
data Pessoa = Ind Nome Sobrenome AnoNascimento
deriving (Show)
type Nome = String 
type Sobrenome = String
type AnoNascimento = Int


instance Eq Est
where
	(==) Verao Verao = True
	(==) Inverno Inverno = True
	(==) Primavera Primavera = True
	(==) Outono Outono = True
	(==) _ _ = False

  -- TIPOS RECURSIVOS
  data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
  eval::Expr->Int
  eval (Lit n) = n
  eval (Add e1 e2) = (eval e1) + (eval e2)
  eval (Sub e1 e2) = (eval e1) - (eval e2)