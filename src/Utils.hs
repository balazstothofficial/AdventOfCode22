module Utils
  ( zipWithNext,
    windowed,
    zipTranspose,
    sortDescending,
    count,
    replace,
    (?),
    traced,
    tracedWithPrefix,
  )
where

import Control.Applicative
import Control.Arrow ((>>>))
import Data.List (intercalate, sortBy, tails)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

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

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace x y = splitOn x >>> intercalate y

infixr 1 ?

(?) :: a -> String -> a
(?) = flip trace

traced :: Show a => a -> a
traced = tracedWithPrefix ""

tracedWithPrefix :: Show a => String -> a -> a
tracedWithPrefix prefix x = x ? prefix ++ show x
