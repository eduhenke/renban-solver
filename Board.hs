module Board (FillAction, Board(Board, cells, areas), newBoardCells, rows, cols, fillCell, isBoardValid) where
  
import Cell ( Cell(..), Fill )  
import Sequence (isSequenceValid)
import Data.List ( transpose )

type Position = (Int, Int)
type FillAction = (Position, Fill)
type Area = [Position]

data Board = Board {
  cells :: [[Cell]],
  areas :: [Area]
}

newBoardCells :: Int -> [[Cell]]
newBoardCells n = replicate n $ replicate n (Cell Nothing)

rows :: Board -> [[Cell]]
rows = cells

cols :: Board -> [[Cell]]
cols = transpose . cells

instance Show Board where
  show board =
    concat $ map (\x -> unwords (map show x) ++ "\n") $ rows board


fillCell :: FillAction -> Board -> Board
fillCell ((x', y'), num') board =
  Board {
    cells=[
      [ replaceCell x y oldPos | (x, oldPos) <- zip [0..] $ row ]
      | (y, row) <- zip [0..] $ rows board
    ],
    areas = areas board
  }
  where replaceCell x y oldPos = if x == x' && y == y' then Cell (Just num') else oldPos

getCellAt :: Board -> Position -> Cell
getCellAt board (x, y) =
  (rows board) !! y !! x

getAreaCells :: Board -> Area -> [Cell]
getAreaCells board area =
  map (getCellAt board) area

isBoardValid :: Board -> Bool
isBoardValid board =
  let validRows = all isSequenceValid $ rows board
      validCols = all isSequenceValid $ cols board
      validAreas = all isSequenceValid $ map (getAreaCells board) $ areas board
  in
    validRows && validCols && validAreas