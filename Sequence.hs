module Sequence (isSequenceValid) where
import Cell

isSequenceValid :: [Cell] -> Bool
isSequenceValid fills =
  let filledNumbers = [num | (Cell (Just num)) <- fills]
      uniqueNumbers = map (\x -> filter ((/=) x)) filledNumbers
      maxNumber = maximum filledNumbers
      minNumber = minimum filledNumbers
      correctLength = length fills
  in
    length uniqueNumbers == correctLength &&
    (maxNumber - minNumber) == (correctLength-1)