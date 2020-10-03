import Board
import Backtrack
import Sequence
import Data.Maybe
board = Board {
  cells=newBoardCells 3,
  areas=
    [ [(0, 0), (0, 1), (1, 0)]
    , [(2, 0), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]]
}
board4 = Board {
  cells=newBoardCells 4,
  areas=
    [ [(0, 0), (0, 1), (0, 2), (1, 0)]
    , [(3, 0), (1, 1), (2, 1), (3, 1)]
    , [(0, 2), (1, 2), (2, 2), (3, 2)]
    , [(0, 3), (1, 3), (2, 3), (3, 3)]]
}

computeActions :: Board -> [FillAction] -> Board
computeActions board actions = foldl fillCell board actions

correctActions :: [FillAction]
correctActions =
  [ ((0, 0), 1), ((1, 0), 3), ((2, 0), 2)
  , ((0, 1), 2), ((1, 1), 1), ((2, 1), 3)
  , ((0, 2), 3), ((1, 2), 2), ((2, 2), 1) ]


correctBoard =
  computeActions board correctActions


incorrectBoardByColRows =
  computeActions board
    [ ((0, 0), 0)
    , ((0, 1), 0)
    , ((0, 2), 0)
    , ((1, 0), 1)
    , ((1, 1), 1)
    , ((1, 2), 1)
    , ((2, 0), 2)
    , ((2, 1), 2)
    , ((2, 2), 2) ]

incorrectBoardByAreas =
  computeActions board
    [ ((0, 0), 2), ((1, 0), 3), ((2, 0), 1)
    , ((0, 1), 3), ((1, 1), 1), ((2, 1), 2)
    , ((0, 2), 1), ((1, 2), 2), ((2, 2), 3) ]

main = do
  -- valid
  print $ not $ isSequenceValid [Nothing]
  print $ not $ isSequenceValid [Nothing, Just 1]
  print $ isSequenceValid [Just 1, Just 2, Just 3]
  print $ isSequenceValid [Just 2, Just 1, Just 3]
  print $ isSequenceValid [Just 2, Just 1, Just 3, Just 4]
  print $ not $ isSequenceValid [Just 2, Just 1, Nothing, Just 3]

  -- invalid
  print $ not $ isSequenceInvalid [Nothing]
  print $ not $ isSequenceInvalid [Nothing, Nothing, Nothing]
  print $ not $ isSequenceInvalid [Nothing, Just 1, Nothing]
  print $ not $ isSequenceInvalid [Nothing, Just 1, Just 2]
  print $ not $ isSequenceInvalid [Just 3, Just 1, Just 2]
  print $ isSequenceInvalid [Just 2, Just 1, Just 2]

  print $ solved correctBoard
  print $ not $ invalid correctBoard
  print $ not $ solved incorrectBoardByColRows
  print $ invalid incorrectBoardByColRows
  print $ not $ solved incorrectBoardByAreas
  print $ invalid incorrectBoardByAreas
  print $ backtrack $ board
  print $ backtrack $ board4
