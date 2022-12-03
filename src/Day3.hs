module Day3 (solution1, solution2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Solution (Solution, solution)
import Data.List (find)
import Data.Maybe (fromJust)

solution1 :: Solution Int
solution1 = solution parse1 solve1

solution2 :: Solution Int
solution2 = solution parse2 solve2

parse1 :: String -> [(String, Set Char)]
parse1 input = parseLine <$> lines input
  where
    parseLine line = (left, Set.fromList right)
      where
        (left, right) = splitAt (length line `div` 2) line

solve1 :: [(String, Set Char)] -> Int
solve1 = solve (\(first, second) -> fromJust $ find (`Set.member` second) first)
    
parse2 :: String -> [(String, Set Char, Set Char)]
parse2 input = group $ lines input
  where    
    group [] = []
    group ls = (first, Set.fromList second, Set.fromList third) : group (drop 3 ls)
      where
        first = head ls
        second = ls !! 1
        third = ls !! 2
        
solve2 :: [(String, Set Char, Set Char)] -> Int
solve2 = solve findCommon
  where
    findCommon (first, second, third) = 
      fromJust $ find (\x -> Set.member x second && Set.member x third) first
      
solve :: (a -> Char) -> [a] -> Int
solve findCommon xs = sum $ score . findCommon <$> xs

score :: Char -> Int
score char = if ascii < 97 then ascii - 38 else ascii - 96
  where
    ascii = fromEnum char
