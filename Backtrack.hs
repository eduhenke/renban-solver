module Backtrack where
import Data.Set
import Data.Foldable
import qualified Data.Set as Set

flatten :: Ord a => Set (Set a) -> Set a
flatten ss' = Set.fold union Set.empty ss'


class Ord state => Backtrackable state where  
  solved :: state -> Bool
  invalid :: state -> Bool
  children :: state -> Set state
  backtrack :: state -> Set state
  backtrack state
    | solved state = singleton state
    | invalid state = empty
    | otherwise = flatten $ Set.map backtrack $ children state
  solve :: state -> Maybe state
  solve state = find (\_ -> True) $ backtrack state