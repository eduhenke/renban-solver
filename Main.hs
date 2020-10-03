import Board

board = Board {
  cells=newBoardCells 3,
  areas=
    [ [(0, 0), (0, 1), (1, 0)]
    , [(2, 0), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]]
}

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldr fillCell board actions

main = do
  print $ board

  print $ computeActions board [((1, 1), 3), ((0, 0), 1)]