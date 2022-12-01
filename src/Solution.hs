{-# LANGUAGE GADTs #-}

module Solution (run, solution, Solution) where

import Control.Arrow ((>>>))

data Solution a where
  Solution :: Show a => {run :: String -> a} -> Solution a

solution :: Show a => (String -> b) -> (b -> a) -> Solution a
solution parse solve = Solution (parse >>> solve)
