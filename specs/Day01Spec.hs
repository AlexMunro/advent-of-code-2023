import Test.Hspec
import Control.Exception
import Day01

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    describe "calibrationValue" $ do
      it "combines the first and last digits of the string" $ do
        calibrationValue "1abc2" `shouldBe` 12
        calibrationValue "pqr3stu8vwx" `shouldBe` 38
        calibrationValue "a1b2c3d4e5f" `shouldBe` 15
        calibrationValue "treb7uchet" `shouldBe` 77

    let input = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

    describe "partOne" $ do
      it "gives you the sum of calibrationValues of supplied lines" $ do
        partOne input `shouldBe` 142

