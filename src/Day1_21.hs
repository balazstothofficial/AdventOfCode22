module Day1_21 (solution1, solution2) where

import Solution (Solution, solution)
import Utils (windowed, zipWithNext)

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

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
