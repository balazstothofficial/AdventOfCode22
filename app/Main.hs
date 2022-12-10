module Main (main) where

import Day9 (solution1, solution2)
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day9" solution2 >>= print
