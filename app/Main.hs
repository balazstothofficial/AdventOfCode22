module Main (main) where

import Day6
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day6" solution2 >>= print
