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
isSequenceInvalid sequence
  | length sequence == length filledNumbers =
    nub filledNumbers /= filledNumbers
  | otherwise = False
  where filledNumbers = catMaybes sequence

areCellsInvalid :: [Cell] -> Bool
areCellsInvalid = isSequenceInvalid . (map filledNumber)