module UtilsTest (utilsTest) where

import Data.List (sort)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Property, property, (==>))
import Utils (sortDescending, zipWithNext, replace)

utilsTest :: SpecWith ()
utilsTest =
  describe "Utils" $
    do
      describe "zip with next" $
        do
          it "length property" $
            property zipWithNextLength
          it "small example" $
            zipWithNext [1 :: Int, 2, 3, 4, 5] `shouldBe` [(1, 2), (2, 3), (3, 4), (4, 5)]

      describe "sort descending" $
        it "reversed property" $
          property sortDescendingReversed

      describe "replace" $
        it "small example" $
          replace "abc" "cba" "xvbabccabcf" `shouldBe` "xvbcbaccbaf"

zipWithNextLength :: [Int] -> Property
zipWithNextLength xs = xs /= [] ==> length xs == length (zipWithNext xs) + 1

sortDescendingReversed :: [Int] -> Bool
sortDescendingReversed xs = reverse (sortDescending xs) == sort xs
