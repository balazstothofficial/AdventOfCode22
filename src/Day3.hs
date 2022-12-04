module Day3 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.Char (isUpper)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Solution (Solution, solution)

solution1 :: Solution Int
solution1 = solution parse1 solve1

solution2 :: Solution Int
solution2 = solution parse2 solve2

parse1 :: String -> [(String, Set Char)]
parse1 = lines >>> fmap parseLine
  where
    parseLine line = (left, Set.fromList right)
      where
        (left, right) = splitAt (length line `div` 2) line

solve1 :: [(String, Set Char)] -> Int
solve1 = solve (\(first, second) -> fromJust $ find (`Set.member` second) first)

parse2 :: String -> [(String, Set Char, Set Char)]
parse2 = lines >>> chunksOf 3 >>> fmap parseGroup
  where
    parseGroup group = (first, Set.fromList second, Set.fromList third)
      where
        first = head group
        second = group !! 1
        third = group !! 2

solve2 :: [(String, Set Char, Set Char)] -> Int
solve2 = solve findCommon
  where
    findCommon (first, second, third) =
      fromJust $ find (\x -> Set.member x second && Set.member x third) first

solve :: (a -> Char) -> [a] -> Int
solve findCommon = fmap (score . findCommon) >>> sum

score :: Char -> Int
score char =
  if isUpper char
    then ascii - fromEnum 'A' + 27
    else ascii - fromEnum 'a' + 1
  where
    ascii = fromEnum char
