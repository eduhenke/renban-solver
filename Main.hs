import Board
import Position
import Sequence

board = Board {
  positions=newBoardPositions 2,
  areas=[]
}

fillActions :: [FillAction]
fillActions = [((1,1), 1), ((1, 3), 4), ((3, 3), 2)]

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldr fillPosition board actions

correctBoard =
  fillPosition ((1, 1), 1) $
  fillPosition ((1, 2), 2) $
  fillPosition ((2, 1), 2) $
  fillPosition ((2, 2), 1) $
  board
incorrectBoard =
  fillPosition ((1, 1), 1) $
  fillPosition ((1, 2), 1) $
  fillPosition ((2, 1), 2) $
  fillPosition ((2, 2), 2) $
  board


main = do
  print $ board
  print $ isBoardValid board
  
  print $ correctBoard
  print $ isBoardValid correctBoard
  
  print $ incorrectBoard
  print $ isBoardValid incorrectBoard
  
  -- print $ computeActions board fillActions