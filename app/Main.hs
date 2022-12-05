module Main (main) where

import Day5
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day5" solution2 >>= print
