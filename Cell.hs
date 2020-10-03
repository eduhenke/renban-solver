module Cell (Fill, Cell(Cell)) where

type Fill = Int

data Cell = Cell (Maybe Fill)

instance Show Cell where
  show (Cell (Just number)) = show number
  show (Cell Nothing) = "-"

