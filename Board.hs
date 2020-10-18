module Board where
  
import Cell
import Sequence
import Data.List ( transpose )
import Backtrack
import qualified Data.Set as Set

type FillAction = (Position, Fill)
-- O tipo Area representa um conjunto específico de células que formam uma área do tabuleiro
type Area = [Position]

type Row = [Cell]

-- Criação de um novo tipo Board e definindo seu construtor
data Board = Board {
  cells :: [Row],
  size :: Int,
  areas :: [Area]
} deriving (Eq, Ord)

-- Criação das células para dado um n temos uma tabela com n linhas e n colunas de células
newBoardCells :: Int -> [[Cell]]
newBoardCells n = [[Cell (x, y) Nothing | x <- [0..(n-1)]] | y <- [0..(n-1)]]

rows :: Board -> [[Cell]]
rows = cells

-- Pegar a matriz transposta do tabuleiro para ter acesso as colunas
cols :: Board -> [[Cell]]
cols = transpose . cells

-- Sobreescrevemos o método Show para quando chamarmos a função print do Board fique de um jeito legível pro usuário o tabuleiro
instance Show Board where
  show board =
    concat $ map (\x -> unwords (map show x) ++ "\n") $ rows board

-- Método para "preencher" uma célula de uma tabela, que dados as células de uma tabela e uma ação de preenchimento, retorna uma nova lista, preenchida.
fillCell :: [[Cell]] -> FillAction -> [[Cell]]
fillCell cells ((x', y'), num') =
  map (map replaceCell) $ cells
  where replaceCell (Cell (x, y) num) = if x == x' && y == y' then Cell (x, y) (Just num') else Cell (x, y) num

-- É um método que dados uma tabela e uma ação de preenchimento retorna a nova tabela, preenchida
fillCellBoard :: Board -> FillAction -> Board
fillCellBoard (Board cells size areas) action =
  Board (fillCell cells action) size areas

-- Retorna a celula na linha x e coluna y da tabela
getCellAt :: [[Cell]] -> Position -> Cell
getCellAt rows (x, y) =
  rows !! y !! x

-- Retorna um array com as células dentro de uma área passada como parâmetro
getAreaCells :: [[Cell]] -> Area -> [Cell]
getAreaCells rows area =
  map (getCellAt rows) area

-- Retorna um array com as células vazias de um Board
emptyCells :: Board -> [Cell]
emptyCells board = filter emptyCell $ concat $ cells board

-- Retorna todas as células das "sequências" (Explicado o que é uma sequência em Sequence.hs)
allCellsSequences :: Board -> [[Cell]]
allCellsSequences board =
  concat [rowsCells, colsCells, areaCells]
  where
    rowsCells = rows board
    colsCells = cols board
    areaCells = map (getAreaCells $ rows board) $ areas board
-- Implementação dos métodos da classe Backtrackable (o método backtrack e solve já estão implementados)
instance Backtrackable Board where
  -- Implementação do método solved, em que dado um board pego todas as células das sequências dele, e vejo se todas as sequências retornadas são válidas (igual a True)
  solved board = all areCellsValid $ allCellsSequences board
  -- Implementação do método invalid, em que se tiver alguma célula vazia ou alguma sequência invalida ele retorna True
  invalid board =
    (length $ emptyCells board) == 0 ||
    (any areCellsInvalid $ allCellsSequences board)
  -- Implementação do método children, que dado um board ele cria as "size" filhos de outros boards com valores novos
  children board =
    let getFillAction cell = [(position cell, num) | num <- [1..(size board)]]
        fillActions = Set.fromList $ concat $ map getFillAction $ [head $ emptyCells board]
    in Set.map (fillCellBoard board) fillActions
    