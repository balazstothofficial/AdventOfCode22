module Day6 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.List (findIndex)
import Solution (Solution, solution)
import Utils (isDistinct, windowed)

solution1 :: Solution (Maybe Int)
solution1 = solution id (solve 4)

solution2 :: Solution (Maybe Int)
solution2 = solution id (solve 14)

solve :: Eq a => Int -> [a] -> Maybe Int
solve n = windowed n >>> findIndex isDistinct >>> fmap (+ n)