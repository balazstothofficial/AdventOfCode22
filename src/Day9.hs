module Day9 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.Foldable (foldl')
import Data.List (nub)
import Solution (Solution, solution)
import Utils ((>$>))

solution1 :: Solution Int
solution1 = solution parse solve1

solution2 :: Solution Int
solution2 = solution parse solve2

solve1 :: [(Int, Int)] -> Int
solve1 = tailPositions >>> nub >>> length

solve2 :: [(Int, Int)] -> Int
solve2 =
  tailPositions
    >>> iterate (positionsToMoves >>> tailPositions)
    >>> (!! 8)
    >>> nub
    >>> length

positionsToMoves :: [(Int, Int)] -> [(Int, Int)]
positionsToMoves positions = zipWith positionToMove r (tail r)
  where
    r = reverse positions
    positionToMove (x, y) (x', y') = (x' - x, y' - y)

tailPositions :: [(Int, Int)] -> [(Int, Int)]
tailPositions = foldl' move ((0, 0), (0, 0), []) >>> (\(_, _, tls) -> tls)
  where
    move (tl, hd, tls) direction = (newTl, newHd, newTl : tls)
      where
        newHd = moveStep hd direction
        newTl = moveTail tl newHd

    moveTail (x, y) (x', y') =
      if max (abs dx) (abs dy) <= 1 then (x, y) else (change dx x, change dy y)
      where
        dx = x' - x
        dy = y' - y

        change d z
          | d < 0 = z - 1
          | 0 < d = z + 1
          | otherwise = z

moveStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveStep (x, y) (dx, dy) = (x + dx, y + dy)

parse :: String -> [(Int, Int)]
parse = lines >$> steps >>> concat
  where
    steps = words >>> parseSteps >>> uncurry expandStep

expandStep :: (Int, Int) -> Int -> [(Int, Int)]
expandStep direction steps = replicate steps direction

parseSteps :: [String] -> ((Int, Int), Int)
parseSteps = splitAt 1 >>> uncurry parseSplitted
  where
    parseSplitted direction steps = (parseDirection $ concat direction, read $ concat steps)

parseDirection :: String -> (Int, Int)
parseDirection "L" = (-1, 0)
parseDirection "R" = (1, 0)
parseDirection "U" = (0, 1)
parseDirection "D" = (0, -1)
parseDirection _ = error "Wrong Direction"
