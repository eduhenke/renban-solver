module Area (Area(Area), isAreaValid) where
import Sequence (isSequenceValid)
import Position ( Position(filledNumber), Fill )

data Area = Area [Position] deriving Show

isAreaValid :: Area -> Bool
isAreaValid (Area positions) = isSequenceValid $ map filledNumber positions