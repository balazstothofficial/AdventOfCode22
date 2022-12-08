module Main (main) where

import Day7 (solution1, solution2)
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day7" solution2 >>= print
