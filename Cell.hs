module Cell where

type Fill = Int
type Position = (Int, Int)

data Cell = Cell Position (Maybe Fill) deriving (Eq, Ord)

emptyCell :: Cell -> Bool
emptyCell (Cell _ Nothing) = True
emptyCell _ = False

position :: Cell -> Position
position (Cell pos _) = pos

filledNumber :: Cell -> Maybe Fill
filledNumber (Cell _ f) = f

instance Show Cell where
  show (Cell _ (Just number)) = show number
  show (Cell _ Nothing) = "-"

