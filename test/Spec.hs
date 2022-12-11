{-# LANGUAGE NumericUnderscores #-}

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import Solution (run, runOnFile)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)
import UtilsTest (utilsTest)

main :: IO ()
main = hspec $
  do
    utilsTest

    describe "Day 1" $
      do
        it "Solution 1" $
          runOnFile "Day1" Day1.solution1 `shouldReturn` 72_718

        it "Solution 2" $
          runOnFile "Day1" Day1.solution2 `shouldReturn` 213_089

    describe "Day 2" $
      do
        it "Solution 1" $
          runOnFile "Day2" Day2.solution1 `shouldReturn` 11_449

        it "Solution 2" $
          runOnFile "Day2" Day2.solution2 `shouldReturn` 13_187

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

    describe "Day 5" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day5_Small" Day5.solution1 `shouldReturn` "CMZ"

        it "Solution 1" $
          runOnFile "Day5" Day5.solution1 `shouldReturn` "HBTMTBSDC"

        it "Solution 2 - Small" $
          runOnFile "Day5_Small" Day5.solution2 `shouldReturn` "MCD"

        it "Solution 2" $
          runOnFile "Day5" Day5.solution2 `shouldReturn` "PQTJRSHWS"

    describe "Day 6" $
      do
        it "Solution 1 - Small" $
          run Day6.solution1 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` Just 5

        it "Solution 1" $
          runOnFile "Day6" Day6.solution1 `shouldReturn` Just 1965

        it "Solution 2" $
          runOnFile "Day6" Day6.solution2 `shouldReturn` Just 2773

    describe "Day 7" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day7_Small" Day7.solution1 `shouldReturn` Right 95_437

        it "Solution 1" $
          runOnFile "Day7" Day7.solution1 `shouldReturn` Right 1_206_825

        it "Solution 2 - Small" $
          runOnFile "Day7_Small" Day7.solution2 `shouldReturn` Right 2_493_3642

        it "Solution 2" $
          runOnFile "Day7" Day7.solution2 `shouldReturn` Right 9_608_311

    describe "Day 8" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day8_Small" Day8.solution1 `shouldReturn` 21

        it "Solution 1" $
          runOnFile "Day8" Day8.solution1 `shouldReturn` 1827

        it "Solution 2 - Small" $
          runOnFile "Day8_Small" Day8.solution2 `shouldReturn` 8

        it "Solution 2" $
          runOnFile "Day8" Day8.solution2 `shouldReturn` 335_580

    describe "Day 9" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day9_Small" Day9.solution1 `shouldReturn` 13

        it "Solution 1" $
          runOnFile "Day9" Day9.solution1 `shouldReturn` 6311

        it "Solution 2 - Small" $
          runOnFile "Day9_Small2" Day9.solution2 `shouldReturn` 36

        it "Solution 2" $
          runOnFile "Day9" Day9.solution2 `shouldReturn` 2482

    describe "Day 10" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day10_Small" Day10.solution1 `shouldReturn` 13140

        it "Solution 1" $
          runOnFile "Day10" Day10.solution1 `shouldReturn` 11220

        it "Solution 2" $
          runOnFile "Day10" Day10.solution2
            `shouldReturn` "###..####.###...##....##.####.#....#..#.\n\
                           \#..#....#.#..#.#..#....#.#....#....#.#..\n\
                           \###....#..#..#.#..#....#.###..#....##...\n\
                           \#..#..#...###..####....#.#....#....#.#..\n\
                           \#..#.#....#....#..#.#..#.#....#....#.#..\n\
                           \###..####.#....#..#..##..####.####.#..#."

    describe "Day 11" $
      do
        it "Solution 1 - Small" $
          runOnFile "Day11_Small" Day11.solution1 `shouldReturn` 10605

        it "Solution 1" $
          runOnFile "Day11" Day11.solution1 `shouldReturn` 98280

        it "Solution 2 - Small" $
          runOnFile "Day11_Small" Day11.solution2 `shouldReturn` 2713310158

        it "Solution 2" $
          runOnFile "Day11" Day11.solution2 `shouldReturn` 17673687232
