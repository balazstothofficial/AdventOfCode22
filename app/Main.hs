module Main (main) where

import DayOne

main :: IO ()
main = readFile "./input/DayOne.txt" >>= print . run2
