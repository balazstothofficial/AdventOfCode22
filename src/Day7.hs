{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Day7 (solution1, solution2) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Either (lefts)
import Data.Either.Combinators (leftToMaybe, mapLeft, swapEither)
import Data.List (find, foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Relude.Monad.Either (maybeToRight)
import Relude.String (readMaybe)
import Safe (minimumMay)
import Solution (Solution, solutionF, solutionM)
import Utils (ifToEither, leftOrRight, mapBoth, orUndo, replaceBy, withInput, (>$>))

data Directory = Directory {directoryName :: String, children :: [File]}
  deriving (Show)

data Data = Data {dataName :: String, dataSize :: Int}
  deriving (Show)

type File = Either Directory Data

data Command = Cd String | CdUp | Ls [File]
  deriving (Show)

type Result a = Either String a

solution1 :: Solution (Result Int)
solution1 = solutionF createFileTree solve1

solution2 :: Solution (Result Int)
solution2 = solutionM createFileTree solve2

solve1 :: Directory -> Int
solve1 = flatten >>> foldl' count 0
  where
    count result =
      Left >>> fileSize >>> ifToEither (<= 100_000) (const result) (+ result) >>> leftOrRight

solve2 :: Directory -> Result Int
solve2 =
  flatten
    >$> (Left >>> fileSize)
    >>> filter (>= 8_381_165)
    >>> minimumMay
    >>> maybeToRight "No big enough file"

fileSize :: File -> Int
fileSize = mapLeft sumChildren >$> dataSize >>> leftOrRight
  where
    sumChildren = children >$> fileSize >>> sum

flatten :: Directory -> [Directory]
flatten directory = directory : flatChildren directory
  where
    flatChildren = children >>> lefts >>> concatMap flatten

createFileTree :: String -> Result Directory
createFileTree = parseCommands >=> flip runCommands (Directory "\\" []) >=> checkExecution
  where
    checkExecution = ifToEither (null . fst) (failure "Did not execute commands: ") snd

runCommands :: [Command] -> Directory -> Result ([Command], Directory)
runCommands [] directory = Right ([], directory)
runCommands (Ls files : commands) directory = runCommands commands directory {children = files}
runCommands (CdUp : commands) directory = Right (commands, directory)
runCommands (Cd targetDirectory : commands) directory = runCd directory
  where
    runCd = withInput subResult >>> uncurry insertSubResult >=> uncurry runCommands
      where
        insertSubResult input result = (fmap . fmap) (insertIn input) result

    subResult =
      children
        >>> findDirectory targetDirectory
        >>> maybeToRight targetNotFound
        >=> runCommands commands

    targetNotFound = "Did not find target for cd " ++ targetDirectory

    insertIn parent child =
      parent {children = replaceBy nameIfDirectory (Left child) (children parent)}

    nameIfDirectory = leftToMaybe >$> directoryName

findDirectory :: String -> [File] -> Maybe Directory
findDirectory name = lefts >>> find sameName
  where
    sameName = directoryName >>> (== name)

parseCommands :: String -> Result [Command]
parseCommands = splitOn "\n$ " >>> drop 1 >$> parseCommand >>> sequence

parseCommand :: String -> Result Command
parseCommand "cd .." = Right CdUp
parseCommand (stripPrefix "cd " -> Just destination) = Right $ Cd destination
parseCommand (stripPrefix "ls\n" -> Just directories) = parseLs directories
  where
    parseLs = lines >$> parseFile >>> sequence >$> Ls
parseCommand wrongCommand = Left $ "Parsing command failed: " ++ wrongCommand

parseFile :: String -> Result File
parseFile =
  orUndo (stripPrefix "dir ")
    >>> swapEither
    >$> parseData
    >>> mapLeft parseDirectory
    >>> sequence
  where
    parseDirectory = flip Directory []

    parseData = orUndo parseDataMaybe >>> mapLeft failParsing

    parseDataMaybe =
      words
        >>> splitAt 1
        >>> mapBoth concat
        >>> swap
        >$> readMaybe
        >>> sequence
        >$> uncurry Data

    failParsing = failure "Parsing file failed: "

failure :: Show a => String -> a -> String
failure message = show >>> (++) message
