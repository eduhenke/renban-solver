module Board (FillAction, Board(Board, cells, areas), newBoardCells, rows, cols, fillCell) where
  
import Cell
import Sequence
import Data.List ( transpose )
import Backtrack
import qualified Data.Set as Set

type FillAction = (Position, Fill)
type Area = [Position]

data Board = Board {
  cells :: [[Cell]],
  areas :: [Area]
} deriving (Eq, Ord)

newBoardCells :: Int -> [[Cell]]
newBoardCells n = [[Cell (x, y) Nothing | x <- [0..(n-1)]] | y <- [0..(n-1)]]

rows :: Board -> [[Cell]]
rows = cells

cols :: Board -> [[Cell]]
cols = transpose . cells

instance Show Board where
  show board =
    concat $ map (\x -> unwords (map show x) ++ "\n") $ rows board


fillCell :: Board -> FillAction -> Board
fillCell board ((x', y'), num') =
  Board {
    cells= map (map replaceCell) $ cells board,
    areas= areas board
  }
  where replaceCell (Cell (x, y) num) = if x == x' && y == y' then Cell (x, y) (Just num') else Cell (x, y) num

getCellAt :: Board -> Position -> Cell
getCellAt board (x, y) =
  (rows board) !! y !! x

getAreaCells :: Board -> Area -> [Cell]
getAreaCells board area =
  map (getCellAt board) area

emptyCells :: Board -> [Cell]
emptyCells board = filter emptyCell $ concat $ cells board

numOptions :: [Fill]
numOptions = [1..4]

allCellsSequences :: Board -> [[Cell]]
allCellsSequences board =
  concat [rowsCells, colsCells, areaCells]
  where
    rowsCells = rows board
    colsCells = cols board
    areaCells = map (getAreaCells board) $ areas board
instance Backtrackable Board where
  solved board = all areCellsValid $ allCellsSequences board
  invalid board =
    (length $ emptyCells board) == 0 ||
    (any areCellsInvalid $ allCellsSequences board)
  children board =
    let getFillAction cell = [(position cell, num) | num <- numOptions]
        fillActions = Set.fromList $ concat $ map getFillAction $ [head $ emptyCells board]
    in Set.map (fillCell board) fillActions
    