module Day1 (solution1, solution2) where

import Data.List.Split (splitOn)
import Relude.Functor.Fmap ((<<$>>))
import Solution (Solution, solution)
import Utils

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

parse :: String -> [[Int]]
parse input = read <<$>> splitOn [""] (lines input)

solve1 :: [[Int]] -> Int
solve1 xs = maximum $ sum <$> xs

solve2 :: [[Int]] -> Int
solve2 xs = sum $ take 3 $ sortDescending $ sum <$> xs
