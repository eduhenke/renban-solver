module Backtrack where
import Data.Set
import Data.Foldable
import qualified Data.Set as Set

-- A escolha do Set em vez de um array, por exemplo, é por que não nos importa a ordem e queremos apenas elementos únicos.

flatten :: Ord a => Set (Set a) -> Set a
flatten ss' = Set.fold union Set.empty ss'


class Ord state => Backtrackable state where  
  -- Método para verificar se dado um estado ele está resolvido
  solved :: state -> Bool
  -- Método para verificar se dado um estado ele está inválido
  invalid :: state -> Bool
  children :: state -> Set state
  backtrack :: state -> Set state
  backtrack state
  -- Caso o estado esteja resolvido retorna o conjunto de um estado
    | solved state = singleton state
  -- Caso o estado esteja inválido retorna um conjunto vazio
    | invalid state = empty
  -- Se ainda não encontrou uma solução e também não é inválido continua criando filhos e mapeando para cada um deles a função backtrack (gerando assim uma árvore de tabuleiros)
  -- O flatten aqui serve pra transformar um conjunto de conjunto de soluções em apenas conjunto de soluções
    | otherwise = flatten $ Set.map backtrack $ children state
  -- Procura a primeira solução válida (igual a True) do conjunto de soluções encontradas pelo backtrack
  solve :: state -> Maybe state
  solve state = find (\_ -> True) $ backtrack state