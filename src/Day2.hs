module Day2 (solution1, solution2) where

import Solution (Solution, solution)

data Hand = Rock | Paper | Scissors
  deriving Eq

solution1 :: Solution Int
solution1 = solution parse1 solve

solution2 :: Solution Int
solution2 = solution (parse parseHand2) solve

parse1 :: String -> [(Hand, Hand)]
parse1 = parse parseHand
  where
    parseHand a b = (parseHand1 a, parseHand1 b)

parse :: (Char -> Char -> (Hand, Hand)) -> String -> [(Hand, Hand)]
parse parseHand input = (\line -> parseHand (head line) (line !! 2)) <$> lines input

parseHand1 :: Char -> Hand
parseHand1 'A' = Rock
parseHand1 'X' = Rock
parseHand1 'B' = Paper
parseHand1 'Y' = Paper
parseHand1 'C' = Scissors
parseHand1 'Z' = Scissors
parseHand1 _ = error "TODO: Handle"

parseHand2 :: Char -> Char -> (Hand, Hand)
parseHand2 a b = (first, second b)
  where
    first = parseHand1 a
    
    second 'X' = inferior first
    second 'Y' = first
    second 'Z' = superior first 
    second _ = error "TODO: Handle"

superior :: Hand -> Hand
superior Rock = Paper
superior Paper = Scissors
superior Scissors = Rock

inferior :: Hand -> Hand
inferior Rock = Scissors
inferior Paper = Rock
inferior Scissors = Paper
     
solve :: [(Hand, Hand)] -> Int
solve xs = sum $ totalScore <$> xs
  where
    totalScore (a, b) = score a b + extraScore b

    score a b
      | inferior a == b = 0
      | a == b = 3
      | otherwise = 6

    extraScore Rock = 1
    extraScore Paper = 2
    extraScore Scissors = 3
