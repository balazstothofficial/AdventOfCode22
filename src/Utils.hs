module Utils
  ( zipWithNext,
    windowed,
    zipTranspose,
    sortDescending,
    count,
    replace,
    replaceElement,
    isDistinct,
    (>$>),
    orUndo,
    mapBoth,
    ifToEither,
    leftOrRight,
    withInput,
    replaceBy,
  )
where

import Control.Applicative
import Control.Arrow ((>>>))
import Data.Either.Combinators (maybeToRight)
import Data.List (intercalate, nub, sortBy, tails)
import Data.List.Split (splitOn)

zipWithNext :: [a] -> [(a, a)]
zipWithNext [] = []
zipWithNext xs = zip xs (tail xs)

zipTranspose :: [[a]] -> [[a]]
zipTranspose = traverse ZipList >>> getZipList

windowed :: Int -> [a] -> [[a]]
windowed m = tails >>> take m >>> zipTranspose

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy $ flip compare

count :: (a -> Bool) -> [a] -> Int
count predicate = filter predicate >>> length

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace x y = splitOn x >>> intercalate y

replaceElement :: Eq a => a -> a -> [a] -> [a]
replaceElement x y = replace [x] [y]

isDistinct :: Eq a => [a] -> Bool
isDistinct xs = nub xs == xs

-- TODO (Probably remove):
infixl 2 >$>

(>$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f >$> g = f >>> fmap g

orUndo :: (a -> Maybe b) -> a -> Either a b
orUndo f a = maybeToRight a $ f a

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (first, second) = (f first, f second)

ifToEither :: (a -> Bool) -> (a -> b) -> (a -> c) -> a -> Either b c
ifToEither predicate left right value =
  if predicate value
    then Right $ right value
    else Left $ left value

leftOrRight :: Either a a -> a
leftOrRight (Left value) = value
leftOrRight (Right value) = value

withInput :: (a -> b) -> a -> (a, b)
withInput f a = (a, f a)

replaceBy :: Eq b => (a -> b) -> a -> [a] -> [a]
replaceBy f x = let b = f x in fmap (\y -> if b == f y then x else y)
