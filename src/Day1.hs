module Day1 (solution1, solution2) where

import Data.List.Split (splitOn)
import Solution (Solution, solution)
import Utils
import Control.Arrow ((>>>))

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

parse :: String -> [[Int]]
parse = lines >>> splitOn [""] >>> (fmap . fmap) read

solve1 :: [[Int]] -> Int
solve1 = solve 1

solve2 :: [[Int]] -> Int
solve2 = solve 3

solve :: Int -> [[Int]] -> Int
solve n = fmap sum >>> sortDescending >>> take n >>> sum
