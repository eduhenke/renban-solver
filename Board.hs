module Board (FillAction, Board(Board, positions, areas), newBoardPositions, rows, cols, fillPosition) where

import Position
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


fillPosition :: Board -> FillAction -> Board
fillPosition board ((x', y'), n) =
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
