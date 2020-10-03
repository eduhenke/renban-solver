import Board
import Control.Exception

board = Board {
  cells=newBoardCells 3,
  areas=
    [ [(0, 0), (0, 1), (1, 0)]
    , [(2, 0), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]]
}

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldr fillCell board actions

correctBoard =
  fillCell ((0, 0), 0) $
  fillCell ((0, 1), 1) $
  fillCell ((0, 2), 2) $
  fillCell ((1, 0), 2) $
  fillCell ((1, 1), 0) $
  fillCell ((1, 2), 1) $
  fillCell ((2, 0), 1) $
  fillCell ((2, 1), 2) $
  fillCell ((2, 2), 0) $
  board

incorrectBoardByColRows =
  fillCell ((0, 0), 0) $
  fillCell ((0, 1), 0) $
  fillCell ((0, 2), 0) $
  fillCell ((1, 0), 1) $
  fillCell ((1, 1), 1) $
  fillCell ((1, 2), 1) $
  fillCell ((2, 0), 2) $
  fillCell ((2, 1), 2) $
  fillCell ((2, 2), 2) $
  board

incorrectBoardByAreas =
  fillCell ((0, 0), 1) $
  fillCell ((0, 1), 2) $
  fillCell ((0, 2), 0) $
  fillCell ((1, 0), 2) $
  fillCell ((1, 1), 0) $
  fillCell ((1, 2), 1) $
  fillCell ((2, 0), 0) $
  fillCell ((2, 1), 1) $
  fillCell ((2, 2), 2) $
  board

main = do
  print $ isBoardValid correctBoard
  print $ not $ isBoardValid incorrectBoardByColRows
  print $ not $ isBoardValid incorrectBoardByAreas
