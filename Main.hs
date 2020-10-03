import Data.List
import Data.Function

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

isFilled :: Position -> Bool
isFilled = (/= Nothing) . filledNumber

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

areaPositions :: Area -> [Position]
areaPositions (Area positions) = positions
isAreaValid :: Area -> Bool
isAreaValid area = isSequenceValid $ map filledNumber $ areaPositions area

data Board = Board {
  positions :: [[Position]],
  areas :: [Area]
}

newBoardPositions :: Int -> [[Position]]
newBoardPositions n = [[emptyPosition x y | x <- [1..n]] | y <- [1..n]]


rows :: Board -> [[Position]]
rows = positions

cols :: Board -> [[Position]]
cols = transpose . positions

instance Show Board where
  show board =
    concat $ map (\x -> unwords (map show x) ++ "\n") $ rows board


fillPosition :: (Int, Int) -> Fill -> Board -> Board
fillPosition (i, j) n board =
  let replacePosition pos = if x pos == i && y pos == j then Position{x=i,y=j,filledNumber=Just n} else pos
  in
    Board {
      positions=map (map replacePosition) $ positions board,
      areas = areas board
    }

board = Board {
  positions=newBoardPositions 3,
  areas=[]
}
main = do
  print $ board
  print $ fillPosition (2,1) 3 $ board
  print $ fillPosition (2,1) 3 $ fillPosition (3,3) 1 $ board