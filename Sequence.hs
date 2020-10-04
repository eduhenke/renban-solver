module Sequence where
import Cell
import Data.Maybe
import Data.List (sort, nub)

isSequenceValid :: [Maybe Fill] -> Bool
isSequenceValid sequence
  | length sequence == length filledNumbers =
    sort filledNumbers == [minimum filledNumbers..maximum filledNumbers]
  | otherwise = False
  where filledNumbers = catMaybes sequence

areCellsValid :: [Cell] -> Bool
areCellsValid = isSequenceValid . (map filledNumber)


isSequenceInvalid :: [Maybe Fill] -> Bool
isSequenceInvalid sequence =
  let filledNumbers = catMaybes sequence
      uniqueNumbers = nub filledNumbers
  in uniqueNumbers /= filledNumbers

areCellsInvalid :: [Cell] -> Bool
areCellsInvalid = isSequenceInvalid . (map filledNumber)