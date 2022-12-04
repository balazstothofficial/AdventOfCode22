{-# LANGUAGE GADTs #-}

{-# OPTIONS -Wno-name-shadowing #-}
module Solution (run, solution, Solution, runOnFile) where

import Control.Arrow ((>>>))

data Solution a where
  Solution :: Show a => {run :: String -> a} -> Solution a

solution :: Show a => (String -> b) -> (b -> a) -> Solution a
solution parse solve = Solution (parse >>> solve)

runOnFile :: String -> Solution b -> IO b
runOnFile name solution = run solution <$> readFile ("./input/" ++ name ++ ".txt")
