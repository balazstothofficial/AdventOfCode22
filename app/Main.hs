module Main (main) where

import Day8 (solution2)
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day8" solution2 >>= print
