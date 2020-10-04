import Board
import Backtrack
import Cell

fillCells :: [[Cell]] -> [FillAction] -> [[Cell]]
fillCells cells actions =
  foldl fillCell cells actions

-- https://www.janko.at/Raetsel/Renban/001.a.htm
boardNum001 = Board {
  cells=fillCells (newBoardCells 6) [
    ((2, 1), 6),
    ((4, 1), 5),
    ((0, 2), 3),
    ((3, 2), 1),
    ((1, 3), 2),
    ((4, 3), 3),
    ((0, 4), 6),
    ((3, 5), 5),
    ((5, 5), 6)
  ],
  size=6,
  areas=
    [
      [(0, 0), (0, 1), (1, 1), (2, 1), (2, 2)],
      [(1, 0)],
      [(2, 0), (3, 0), (3, 1), (4, 1)],
      [(4, 0)],
      [(5, 0), (5, 1)],
      [(0, 2), (0, 3), (1, 3)],
      [(1, 2)],
      [(3, 2)],
      [(4, 2), (5, 2)],
      [(2, 3), (3, 3), (4, 3), (5, 3)],
      [(0, 4), (1, 4), (1, 5)],
      [(2, 4)],
      [(3, 4), (2, 5), (3, 5)],
      [(4, 4), (5, 4), (4, 5)],
      [(0, 5)],
      [(5, 5)]
    ]
}

main = do
  print $ boardNum001
  -- see board-num-001.png
  print $ solve boardNum001