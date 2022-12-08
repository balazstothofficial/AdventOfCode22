{-# LANGUAGE GADTs #-}

{-# OPTIONS -Wno-name-shadowing #-}
module Solution (run, solution, solutionF, solutionM, Solution, runOnFile) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Utils ((>$>))

data Solution a where
  Solution :: Show a => {run :: String -> a} -> Solution a

solution :: Show a => (String -> b) -> (b -> a) -> Solution a
solution parse solve = Solution (parse >>> solve)

solutionF :: (Show (f a), Functor f) => (String -> f b) -> (b -> a) -> Solution (f a)
solutionF parse solve = Solution (parse >$> solve)

solutionM :: (Show (m a), Monad m) => (String -> m b) -> (b -> m a) -> Solution (m a)
solutionM parse solve = Solution (parse >=> solve)

runOnFile :: String -> Solution b -> IO b
runOnFile name solution = run solution <$> readFile ("./input/" ++ name ++ ".txt")
