module Board where
  
import Cell
import Sequence
import Data.List ( transpose )
import Backtrack
import qualified Data.Set as Set

type FillAction = (Position, Fill)
type Area = [Position]

data Board = Board {
  cells :: [[Cell]],
  size :: Int,
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


fillCell :: [[Cell]] -> FillAction -> [[Cell]]
fillCell cells ((x', y'), num') =
  map (map replaceCell) $ cells
  where replaceCell (Cell (x, y) num) = if x == x' && y == y' then Cell (x, y) (Just num') else Cell (x, y) num

fillCellBoard :: Board -> FillAction -> Board
fillCellBoard (Board cells size areas) action =
  Board (fillCell cells action) size areas

getCellAt :: [[Cell]] -> Position -> Cell
getCellAt rows (x, y) =
  rows !! y !! x

getAreaCells :: [[Cell]] -> Area -> [Cell]
getAreaCells rows area =
  map (getCellAt rows) area

emptyCells :: Board -> [Cell]
emptyCells board = filter emptyCell $ concat $ cells board

allCellsSequences :: Board -> [[Cell]]
allCellsSequences board =
  concat [rowsCells, colsCells, areaCells]
  where
    rowsCells = rows board
    colsCells = cols board
    areaCells = map (getAreaCells $ rows board) $ areas board
instance Backtrackable Board where
  solved board = all areCellsValid $ allCellsSequences board
  invalid board =
    (length $ emptyCells board) == 0 ||
    (any areCellsInvalid $ allCellsSequences board)
  children board =
    let getFillAction cell = [(position cell, num) | num <- [1..(size board)]]
        fillActions = Set.fromList $ concat $ map getFillAction $ [head $ emptyCells board]
    in Set.map (fillCellBoard board) fillActions
    