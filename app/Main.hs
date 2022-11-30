module Main (main) where

import DayOne21

main :: IO ()
main = readFile "./input/DayOne21.txt" >>= print . run2
