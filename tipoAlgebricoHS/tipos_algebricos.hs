type Nome = String
type Linguagem = String
type Universidade = String

data Pessoa = Programador Nome Linguagem | Aluno Nome Universidade deriving(Show)

programador = Programador "Marcos" "Haskell"
aluno = Aluno "Marcos" "UFPI"

eh_programador :: Pessoa -> Bool
eh_programador (Programador _ _) = True
eh_programador _ = False

eh_aluno :: Pessoa -> Bool
eh_aluno (Aluno _ _) = True
eh_aluno _ = False