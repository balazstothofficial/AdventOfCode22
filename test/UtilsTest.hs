module UtilsTest (utilsTest) where

import Data.List (sort)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Property, property, (==>))
import Utils (sortDescending, zipWithNext)

utilsTest :: SpecWith ()
utilsTest =
  describe "Utils" $
    do
      describe "zipWithNext" $
        do
          it "length property" $
            property zipWithNextLength
          it "small example" $
            zipWithNext [1 :: Int, 2, 3, 4, 5] `shouldBe` [(1, 2), (2, 3), (3, 4), (4, 5)]
      describe "sort descending" $
        do
          it "reversed property" $
            property sortDescendingReversed

zipWithNextLength :: [Int] -> Property
zipWithNextLength xs = xs /= [] ==> length xs == length (zipWithNext xs) + 1

sortDescendingReversed :: [Int] -> Bool
sortDescendingReversed xs = reverse (sortDescending xs) == sort xs
