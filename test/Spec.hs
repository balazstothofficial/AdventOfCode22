import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import Solution (runOnFile)
import Test.Hspec (describe, hspec, it, shouldReturn)
import UtilsTest (utilsTest)

main :: IO ()
main = hspec $
  do
    utilsTest

    describe "Day 1" $
      do
        it "Solution 1" $
          runOnFile "Day1" Day1.solution1 `shouldReturn` 72718

        it "Solution 2" $
          runOnFile "Day1" Day1.solution2 `shouldReturn` 213089

    describe "Day 2" $
      do
        it "Solution 1" $
          runOnFile "Day2" Day2.solution1 `shouldReturn` 11449

        it "Solution 2" $
          runOnFile "Day2" Day2.solution2 `shouldReturn` 13187

    describe "Day 3" $
      do
        it "Solution 1" $
          runOnFile "Day3" Day3.solution1 `shouldReturn` 7903

        it "Solution 2" $
          runOnFile "Day3" Day3.solution2 `shouldReturn` 2548

    describe "Day 4" $
      do
        it "Solution 1" $
          runOnFile "Day4" Day4.solution1 `shouldReturn` 532

        it "Solution 2" $
          runOnFile "Day4" Day4.solution2 `shouldReturn` 854
