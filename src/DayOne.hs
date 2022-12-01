module DayOne (run1, run2) where

import Control.Arrow ((>>>))
import Data.List.Split (splitOn)
import Relude.Functor.Fmap ((<<$>>))
import Utils

run1 :: String -> Int
run1 = run solve1

run2 :: String -> Int
run2 = run solve2

run :: ([[Int]] -> Int) -> String -> Int
run solve = parse >>> solve

parse :: String -> [[Int]]
parse input = read <<$>> splitOn [""] (lines input)

solve1 :: [[Int]] -> Int
solve1 xs = maximum $ sum <$> xs

solve2 :: [[Int]] -> Int
solve2 xs = sum $ take 3 $ sortDescending $ sum <$> xs
