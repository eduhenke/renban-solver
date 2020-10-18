module Cell where

type Fill = Int
type Position = (Int, Int)

-- Criação de um novo tipo Cell e definindo seu construtor, uma célula é uma posição com um possível valor de inteiro e com as operações de igualdade e ordenação.
data Cell = Cell Position (Maybe Fill) deriving (Eq, Ord)

-- Verifica se uma célula está vazia (sem valor atrelado)
emptyCell :: Cell -> Bool
emptyCell (Cell _ Nothing) = True
emptyCell _ = False

-- Método para retornar a posição de uma célula
position :: Cell -> Position
position (Cell pos _) = pos

-- Método pra retornar o valor atrelado a uma célula, sendo que este pode ser nulo.
filledNumber :: Cell -> Maybe Fill
filledNumber (Cell _ f) = f

-- Sobreescrevemos o método Show para quando printarmos uma célula ter a formatação mais legível para o usuário
instance Show Cell where
  show (Cell _ (Just number)) = show number
  show (Cell _ Nothing) = "-"

