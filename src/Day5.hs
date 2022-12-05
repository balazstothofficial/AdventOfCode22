module Day5 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import GHC.Arr (Array, elems, listArray, (!), (//))
import Solution (Solution, solution)

newtype Crate = Crate {content :: Char}
  deriving (Show)

data Move = Move Int Int Int
  deriving (Show)

solution1 :: Solution String
solution1 = solution parse $ uncurry solve1

solution2 :: Solution String
solution2 = solution parse $ uncurry solve2

parse :: String -> (Array Int [Crate], [Move])
parse input = (parseCrates $ head parts, parseMoves $ parts !! 1)
  where
    parts = splitOn "\n\n" input

parseCrates :: String -> Array Int [Crate]
parseCrates = lines >>> init >>> fmap parseCratesLine >>> transpose >>> fmap catMaybes >>> array
  where
    parseCratesLine ('[' : c : ']' : xs) = Just (Crate c) : parseCratesLine (drop 1 xs)
    parseCratesLine (' ' : ' ' : ' ' : xs) = Nothing : parseCratesLine (drop 1 xs)
    parseCratesLine _ = []

    array list = listArray (0, length list - 1) list

parseMoves :: String -> [Move]
parseMoves = lines >>> fmap parseMove

parseMove :: String -> Move
parseMove input = Move (part 1) (part 3 - 1) (part 5 - 1)
  where
    part = (parts !!) >>> read
    parts = splitOn " " input

moveCrate :: ([Crate] -> [Crate] -> [Crate]) -> Array Int [Crate] -> Move -> Array Int [Crate]
moveCrate transfer crates (Move n from to) = crates // [(from, fromCrates), (to, toCrates)]
  where
    (transferable, fromCrates) = splitAt n $ crates ! from
    toCrates = transfer transferable $ crates ! to

solve :: ([Crate] -> [Crate] -> [Crate]) -> Array Int [Crate] -> [Move] -> String
solve transfer crates = foldl (moveCrate transfer) crates >>> elems >>> fmap (content . head)

solve1 :: Array Int [Crate] -> [Move] -> String
solve1 = solve $ flip $ foldl $ flip (:)

solve2 :: Array Int [Crate] -> [Move] -> String
solve2 = solve (++)
