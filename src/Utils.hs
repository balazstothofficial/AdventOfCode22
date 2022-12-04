module Utils (zipWithNext, windowed, zipTranspose, sortDescending, count) where

import Control.Applicative
import Data.List (sortBy, tails)

zipWithNext :: [a] -> [(a, a)]
zipWithNext [] = []
zipWithNext xs = zip xs (tail xs)

zipTranspose :: [[a]] -> [[a]]
zipTranspose = getZipList . traverse ZipList

windowed :: Int -> [a] -> [[a]]
windowed m = zipTranspose . take m . tails

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy $ flip compare

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate
