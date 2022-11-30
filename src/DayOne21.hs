module DayOne21 (run1, run2) where

import Utils

run1 :: String -> Int
run1 = run solve1

run2 :: String -> Int
run2 = run solve2

run :: ([Int] -> Int) -> String -> Int
run solve = solve . parse

parse :: String -> [Int]
parse input = read <$> lines input

solve1 :: [Int] -> Int
solve1 xs = foldl count 0 $ zipWithNext xs
  where
    count acc (x, y) = if x < y then acc + 1 else acc
    
solve2 :: [Int] -> Int
solve2 = solve1 . aggregate
  where
    aggregate xs = sum <$> windowed 3 xs
