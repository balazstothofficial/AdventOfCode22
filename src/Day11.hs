{-# LANGUAGE ViewPatterns #-}

module Day11 (solution1, solution2) where

import Control.Arrow ((>>>))
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Solution (Solution, solution)
import Utils (sortDescending, (>$>))

newtype Monkey = Monkey Integer
  deriving (Eq, Ord, Show)

newtype Item = Item Integer

type Items = Map Monkey (Seq Item)

type Action = (Integer -> Integer) -> Item -> (Monkey, Item)

type Actions = Map Monkey Action

solution1 :: Solution Integer
solution1 = solution parse (\(items, actions, _) -> solve items actions (`div` 3) 20)

solution2 :: Solution Integer
solution2 = solution parse (\(items, actions, base) -> solve items actions (`mod` base) 10000)

-- TODO: type Result a = Either String a

solve :: Items -> Actions -> (Integer -> Integer) -> Int -> Integer
solve items actions reduction rounds = total
  where
    total = product $ take 2 $ sortDescending $ Map.elems $ foldl (Map.unionWith (+)) monkeysWithZero thrown
    thrown = zipWith (Map.unionWith (\a b -> if a > b then a - b else 0)) numbers (tail numbers)
    numbers = fmap itemNumbers results
    results = play items actions reduction rounds
    monkeysWithZero = Map.map (const 0) actions

itemNumbers :: Items -> Map Monkey Integer
itemNumbers = Map.map (toInteger . Seq.length)

play :: Items -> Actions -> (Integer -> Integer) -> Int -> [Items]
play items actions reduction rounds = scanl (turn actions reduction) items order
  where
    monkeys = Map.keys actions
    order = take (length monkeys * rounds) (cycle monkeys)

turn :: Actions -> (Integer -> Integer) -> Items -> Monkey -> Items
turn actions reduction allItems monkey = Map.insert monkey Seq.empty updatedItems
  where
    updatedItems =
      foldl (\acc (monkey', item) -> Map.adjust (|> item) monkey' acc) allItems thrownItems
    thrownItems = fmap (monkeyAction reduction) items
    items = fromJust $ Map.lookup monkey allItems
    monkeyAction = fromJust $ Map.lookup monkey actions

parse :: String -> (Items, Actions, Integer)
parse = splitOn "\n\n" >$> parseMonkey >>> fork3 items actions base
  where
    items = fmap dropAction >>> Map.fromList
    dropAction (monkey, monkeyItems, _, _) = (monkey, monkeyItems)

    actions = fmap dropItems >>> Map.fromList
    dropItems (monkey, _, action, _) = (monkey, action)

    base = foldl (\acc (_, _, _, divisor) -> acc * divisor) 1

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork first second x = (first x, second x)

fork3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
fork3 first second third x = (first x, second x, third x)

parseMonkey :: String -> (Monkey, Seq Item, Action, Integer)
parseMonkey input = (Monkey monkeyId, items, nextMonkey, divisor)
  where
    monkeyId = parseMonkeyId $ head inputLines
    items = parseItemLine $ inputLines !! 1

    nextMonkey reduction (Item item) = (Monkey destination, Item newItem)
      where
        newItem = reduction $ operation item
        destination = if test newItem then destinationIfTrue else destinationIfFalse

    operation = parseOperationLine $ inputLines !! 2
    (test, divisor) = parseTestLine $ inputLines !! 3
    destinationIfTrue = parseDestination $ inputLines !! 4
    destinationIfFalse = parseDestination $ inputLines !! 5

    inputLines = lines input

parseMonkeyId :: String -> Integer
parseMonkeyId (stripPrefix "Monkey " -> Just monkeyId) = read $ init monkeyId
parseMonkeyId monkeyId = error $ "Parsing id failed: " ++ monkeyId

parseItemLine :: String -> Seq Item
parseItemLine (stripPrefix "  Starting items: " -> Just items) = parseItemList items
  where
    parseItemList = splitOn ", " >$> (Item . read) >>> Seq.fromList
parseItemLine items = error $ "Parsing items failed: " ++ items

parseOperationLine :: String -> (Integer -> Integer)
parseOperationLine (stripPrefix "  Operation: new = " -> Just operationString) = operation
  where
    operation x = operand1 x `operator` operand2 x

    operand1 = parseOperand $ head operationElements
    operator = parseOperator $ operationElements !! 1
    operand2 = parseOperand $ operationElements !! 2

    operationElements = words operationString

    parseOperand "old" = id
    parseOperand n = const $ read n

    parseOperator "*" = (*)
    parseOperator "+" = (+)
    parseOperator op = error $ "Parsing operator failed: " ++ op
parseOperationLine operation = error $ "Parsing opertaion failed: " ++ operation

parseTestLine :: String -> (Integer -> Bool, Integer)
parseTestLine (stripPrefix "  Test: divisible by " -> Just divisor) =
  (\x -> x `mod` read divisor == 0, read divisor)
parseTestLine test = error $ "Parsing test line failed: " ++ test

parseDestination :: String -> Integer
parseDestination (stripPrefix "    If true: throw to monkey " -> Just destination) =
  read destination
parseDestination (stripPrefix "    If false: throw to monkey " -> Just destination) =
  read destination
parseDestination destination = error $ "Parsing destination line failed: " ++ destination
