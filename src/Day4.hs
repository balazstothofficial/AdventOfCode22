module Day4 (solution1, solution2) where

import Data.List.Split (splitOn)
import Solution (Solution, solution)
import Utils (count)
import Control.Arrow ((>>>))

data Range = ClosedRange Int Int

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

parse :: String -> [(Range, Range)]
parse = lines >>> fmap parseRanges
  where
    parseRanges string = (range 0, range 1)
      where
        range i = parseRange $ ranges !! i
        ranges = splitOn "," string

    parseRange string = ClosedRange (end 0) (end 1)
      where
        end i = read $ ends !! i
        ends = splitOn "-" string

solve1 :: [(Range, Range)] -> Int
solve1 = count fullyOverlap

solve2 :: [(Range, Range)] -> Int
solve2 = count overlap

fullyOverlap :: (Range, Range) -> Bool
fullyOverlap (ClosedRange start1 end1, ClosedRange start2 end2) =
  start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1

overlap :: (Range, Range) -> Bool
overlap (ClosedRange start1 end1, ClosedRange start2 end2) =
  start1 >= start2 && start1 <= end2 || start2 >= start1 && start2 <= end1
