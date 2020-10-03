import Board

board = Board {
  positions=newBoardPositions 3,
  areas=[]
}

fillActions :: [FillAction]
fillActions = [((1,1), 1), ((1, 3), 4), ((3, 3), 2)]

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldl fillPosition board actions
main = do
  print $ board
  print $ fillPosition board ((2,1), 3)
  print $ computeActions board fillActions