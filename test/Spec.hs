import Test.Hspec (hspec)
import UtilsTest (utilsTest)

main :: IO ()
main = hspec $
  do
    utilsTest
    -- TODO: Test for days
