module Utils (zipWithNext, transpose, windowed, sortDescending) where

import Control.Applicative
import Data.List (sortBy, tails)

zipWithNext :: [a] -> [(a, a)]
zipWithNext [] = []
zipWithNext xs = zip xs (tail xs)

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

windowed :: Int -> [a] -> [[a]]
windowed m = transpose . take m . tails

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy $ flip compare
