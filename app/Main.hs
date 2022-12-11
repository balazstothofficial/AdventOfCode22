module Main (main) where

import Day11 (solution1, solution2)
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day11" solution2 >>= print
