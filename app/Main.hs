module Main (main) where

import DayTwo
import Solution (Solution)
import qualified Solution (run)

main :: IO ()
main = run "DayTwo.txt" solution2

run :: Show b => String -> Solution b -> IO ()
run fileName solution = readInput fileName >>= showSolution solution

readInput :: String -> IO String
readInput name = readFile $ "./input/" ++ name

showSolution :: Show b => Solution b -> String -> IO ()
showSolution solution = print . Solution.run solution
