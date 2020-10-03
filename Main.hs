import Board
import Position
import Sequence

board = Board {
  positions=newBoardPositions 3,
  areas=
    [ [(0, 0), (0, 1), (1, 0)]
    , [(2, 0), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]]
}

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldr fillPosition board actions

correctBoard =
  fillPosition ((0, 0), 0) $
  fillPosition ((0, 1), 1) $
  fillPosition ((0, 2), 2) $
  fillPosition ((1, 0), 2) $
  fillPosition ((1, 1), 0) $
  fillPosition ((1, 2), 1) $
  fillPosition ((2, 0), 1) $
  fillPosition ((2, 1), 2) $
  fillPosition ((2, 2), 0) $
  board

incorrectBoardByColRows =
  fillPosition ((0, 0), 0) $
  fillPosition ((0, 1), 0) $
  fillPosition ((0, 2), 0) $
  fillPosition ((1, 0), 1) $
  fillPosition ((1, 1), 1) $
  fillPosition ((1, 2), 1) $
  fillPosition ((2, 0), 2) $
  fillPosition ((2, 1), 2) $
  fillPosition ((2, 2), 2) $
  board

incorrectBoardByAreas =
  fillPosition ((0, 0), 1) $
  fillPosition ((0, 1), 2) $
  fillPosition ((0, 2), 0) $
  fillPosition ((1, 0), 2) $
  fillPosition ((1, 1), 0) $
  fillPosition ((1, 2), 1) $
  fillPosition ((2, 0), 0) $
  fillPosition ((2, 1), 1) $
  fillPosition ((2, 2), 2) $
  board


main = do
  print $ board
  print $ isBoardValid board
  
  print $ correctBoard
  print $ isBoardValid correctBoard
  
  print $ incorrectBoardByColRows
  print $ isBoardValid incorrectBoardByColRows

  print $ incorrectBoardByAreas
  print $ isBoardValid incorrectBoardByAreas


  -- print $ computeActions board fillActions