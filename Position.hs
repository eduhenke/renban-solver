module Position (Fill, Position(Position, x, y, filledNumber), emptyPosition) where

type Fill = Int

data Position = Position {
  x :: Int,
  y :: Int,
  filledNumber :: Maybe Fill
}

instance Show Position where
  show position =
    case filledNumber position of
      Just number -> show number
      Nothing -> "-"

emptyPosition :: Int -> Int -> Position
emptyPosition x y = Position {x=x, y=y, filledNumber=Nothing}
