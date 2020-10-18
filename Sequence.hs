module Sequence where
import Cell
import Data.Maybe
import Data.List (sort, nub)

-- Verfica se a linha, coluna ou a área não tem celulas vazias e se sequência não repetida e consecutiva
isSequenceValid :: [Maybe Fill] -> Bool
isSequenceValid sequence
  | length sequence == length filledNumbers =
    sort filledNumbers == [minimum filledNumbers..maximum filledNumbers]
  | otherwise = False
  where filledNumbers = catMaybes sequence

-- Recebe uma lista de células e retornas um booleano indicando se a sequência é válida.
areCellsValid :: [Cell] -> Bool
areCellsValid = isSequenceValid . (map filledNumber)


-- Verfica se a linha, coluna ou a área não tem celulas com valores repetidos ou não consecutivo
isSequenceInvalid :: [Maybe Fill] -> Bool
isSequenceInvalid sequence =
  let filledNumbers = catMaybes sequence
      uniqueNumbers = nub filledNumbers
  in uniqueNumbers /= filledNumbers

-- Recebe uma lista de células e retornas um booleano indicando se a sequência é inválida.
areCellsInvalid :: [Cell] -> Bool
areCellsInvalid = isSequenceInvalid . (map filledNumber)