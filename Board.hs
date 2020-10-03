module Board (FillAction, Board(Board, positions, areas), newBoardPositions, rows, cols, fillPosition, isBoardValid) where
  
import Position  
import Sequence (isSequenceValid)
import Area
import Data.List
import Data.Function

type FillAction = ((Int, Int), Fill)

data Board = Board {
  positions :: [[Position]],
  areas :: [Area]
}

newBoardPositions :: Int -> [[Position]]
newBoardPositions n = [[emptyPosition x y | x <- [1..n]] | y <- [1..n]]

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

isBoardValid :: Board -> Bool
isBoardValid board =
  let areAllPositionsValid ps = all isSequenceValid $ map (map filledNumber) ps
      validRows = areAllPositionsValid $ rows board
      validCols = areAllPositionsValid $ cols board
      validAreas = True -- TODO
  in
    validRows && validCols