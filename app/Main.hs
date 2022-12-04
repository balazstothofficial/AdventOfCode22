module Main (main) where

import Day4
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day4" solution2 >>= print
