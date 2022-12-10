module Main (main) where

import Day10 (solution2)
import Solution (runOnFile)

main :: IO ()
main = runOnFile "Day10" solution2 >>= putStrLn
