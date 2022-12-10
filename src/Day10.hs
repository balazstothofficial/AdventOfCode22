module Day10 (solution1, solution2) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Foldable.WithIndex (ifoldl)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Solution (Solution, solution)
import Prelude hiding (cycle)

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution String
solution2 = solution parse solve2

solve1 :: [Int -> Int] -> Int
solve1 = ifoldl step (0, 1, [20, 60, 100, 140, 180, 220]) >>> (\(signal, _, _) -> signal)
  where
    step i (signal, x, c : cs) cycle = (signal', x', cs')
      where
        x' = cycle x
        (signal', cs') =
          if (i + 2) == c
            then (signal + c * x', cs)
            else (signal, c : cs)
    step _ acc _ = acc

solve2 :: [Int -> Int] -> String
solve2 =
  ifoldl step (1, [])
    >>> snd
    >>> reverse
    >>> chunksOf (40 * 6)
    >>> fmap (chunksOf 40)
    >>> fmap (intercalate "\n")
    >>> intercalate "\n"
  where
    step i (x, pixels) cycle = (cycle x, pixels')
      where
        position = i `mod` 40
        
        pixels' =
          if position >= x - 1 && position <= x + 1
            then '#' : pixels
            else '.' : pixels

parse :: String -> [Int -> Int]
parse = lines >=> parseLine
  where
    parseLine "noop" = [id]
    parseLine line = id : [num line]
      where
        num = words >>> (!! 1) >>> read >>> (+)
