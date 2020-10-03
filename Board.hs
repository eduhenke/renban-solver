module Board (FillAction, Board(Board, positions, areas), newBoardPositions, rows, cols, fillPosition, isBoardValid) where
  
import Position  
import Sequence (isSequenceValid)
import Data.List
import Data.Function

type FillAction = ((Int, Int), Fill)
type Area = [(Int, Int)]
data Board = Board {
  positions :: [[Position]],
  areas :: [Area]
}

newBoardPositions :: Int -> [[Position]]
newBoardPositions n = [[emptyPosition x y | x <- [0..(n-1)]] | y <- [0..(n-1)]]

rows :: Board -> [[Position]]
rows = positions

cols :: Board -> [[Position]]
cols = transpose . positions

instance Show Board where
  show board =
    concat $ map (\x -> unwords (map show x) ++ "\n") $ rows board


fillPosition :: FillAction -> Board -> Board
fillPosition ((x', y'), n) board =
  let
    replacePosition p =
      if x p == x' && y p == y' then
        Position{ x=x', y=y', filledNumber=Just n}
      else
        p
  in
    Board {
      positions=map (map replacePosition) $ positions board,
      areas = areas board
    }

getPositionAt :: (Int, Int) -> Board -> Position
getPositionAt (i,j) board =
  (positions board) !! j !! i

getAreaPositions :: Area -> Board -> [Position]
getAreaPositions area board =
  map (\p -> getPositionAt p board) area

isBoardValid :: Board -> Bool
isBoardValid board =
  let areAllPositionsValid ps = all isSequenceValid $ map (map filledNumber) ps
      validRows = areAllPositionsValid $ rows board
      validCols = areAllPositionsValid $ cols board
      validAreas = areAllPositionsValid $ map (\a -> getAreaPositions a board) $ areas board
  in
    validRows && validCols && validAreas