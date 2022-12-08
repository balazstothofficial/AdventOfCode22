module Debug ((?), traced, tracedWithPrefix) where

import Debug.Trace (trace)

infixr 1 ?

(?) :: a -> String -> a
(?) = flip trace

traced :: Show a => a -> a
traced = tracedWithPrefix ""

tracedWithPrefix :: Show a => String -> a -> a
tracedWithPrefix prefix x = x ? prefix ++ show x
