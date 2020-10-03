module Sequence (isSequenceValid) where
import Position (Fill)

isSequenceValid :: [Maybe Fill] -> Bool
isSequenceValid fills =
  let filledNumbers = [num | Just num <- fills]
      uniqueNumbers = map (\x -> filter ((/=) x)) filledNumbers
      maxNumber = maximum filledNumbers
      minNumber = minimum filledNumbers
      correctLength = length fills
  in
    length uniqueNumbers == correctLength &&
    (maxNumber - minNumber) == (correctLength-1)