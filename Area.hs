module Area (Area(Area), isSequenceValid, isAreaValid) where
  
import Position ( Position(filledNumber), Fill )

data Area = Area [Position] deriving Show

isSequenceValid :: [Maybe Fill] -> Bool
isSequenceValid fills =
  let filledNumbers = [num | Just num <- fills]
      uniqueNumbers = map (\x -> filter ((/=) x)) filledNumbers
      maxNumber = maximum filledNumbers
      minNumber = minimum filledNumbers
      correctLength = length fills
  in
    length uniqueNumbers == correctLength &&
    (maxNumber - minNumber) == correctLength

isAreaValid :: Area -> Bool
isAreaValid (Area positions) = isSequenceValid $ map filledNumber positions