{-# LANGUAGE ScopedTypeVariables #-}

module Day8 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.Foldable.WithIndex (ifoldl')
import Data.Functor.WithIndex (imap)
import Data.List (foldl', transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.Tuple (swap)
import Solution (Solution, solution)
import Utils ((>$>))

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

parse :: String -> [[Int]]
parse = lines >$> fmap (digit . fromEnum)
  where
    digit x = x - fromEnum '0'

solve1 :: [[Int]] -> Int
solve1 = visibleTrees >>> Set.size

solve2 :: [[Int]] -> Int
solve2 = scenicScores >>> maximum

visibleTrees :: [[Int]] -> Set (Int, Int)
visibleTrees = fromAllSides Set.union visibilities

scenicScores :: [[Int]] -> Map (Int, Int) Int
scenicScores = fromAllSides (Map.unionWith (*)) viewDistances

visibilities :: ((Int, Int) -> (Int, Int)) -> Int -> [Int] -> Set (Int, Int)
visibilities mapIndices rowIndex = ifoldl' step (-1, Set.empty) >>> snd
  where
    step index (maxHeight, found) current =
      if current > maxHeight
        then (current, Set.insert (mapIndices (rowIndex, index)) found)
        else (maxHeight, found)

viewDistances :: ((Int, Int) -> (Int, Int)) -> Int -> [Int] -> Map (Int, Int) Int
viewDistances mapIndices rowIndex =
  ifoldl' step (Map.empty, insertAll [0 .. 9] 1 Map.empty) >>> fst
  where
    step index (distances, possibleDistances) current =
      if index == 0
        then (Map.insert indices 0 distances, possibleDistances)
        else
          let distance = Map.findWithDefault 1 current possibleDistances
              newDistances = Map.insert indices distance distances
              newPossibleDistances = Map.mapWithKey updatePossibleDistances possibleDistances
           in (newDistances, newPossibleDistances)
      where
        indices = mapIndices (rowIndex, index)
        updatePossibleDistances height distance =
          if height <= current then 1 else distance + 1

fromAllSides ::
  forall s. (s -> s -> s) -> (((Int, Int) -> (Int, Int)) -> Int -> [Int] -> s) -> [[Int]] -> s
fromAllSides combine collect grid = combine (horizontal id grid) (horizontal swap (transpose grid))
  where
    horizontal :: ((Int, Int) -> (Int, Int)) -> [[Int]] -> s
    horizontal mapIndices = imap collectHorizontal >>> foldl1 combine
      where
        collectHorizontal rowIndex row = combine fromLeft fromRight
          where
            fromLeft = collect mapIndices rowIndex row
            fromRight = collect (mapIndices . reverseIndex) rowIndex (reverse row)

            reverseIndex (x, y) = (x, rowLength - y - 1)
            rowLength = length row

insertAll :: Ord k => [k] -> a -> Map k a -> Map k a
insertAll keys value m = foldl' (\acc key -> Map.insert key value acc) m keys
