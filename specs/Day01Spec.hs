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

    describe "wordyCalibrationValue" $ do
      it "combines the first and last digits or word numbers of the string" $ do
        wordyCalibrationValue "two1nine" `shouldBe` 29
        wordyCalibrationValue "eighttwothree" `shouldBe` 83
        wordyCalibrationValue "abcone2threexyz" `shouldBe` 13
        wordyCalibrationValue "xtwone3four" `shouldBe` 24
        wordyCalibrationValue "4nineeightsecen2" `shouldBe` 42
        wordyCalibrationValue "zoneight234" `shouldBe` 14
        wordyCalibrationValue "7pqrstsixteen" `shouldBe` 76

    describe "partOne" $ do
      let input = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
  
      it "gives you the sum of calibrationValues of supplied lines" $ do
        partOne input `shouldBe` 142

    describe "partTwo" $ do
      let input = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

      it "gives you the sum of wordyCalibrationValues of supplied lines" $ do
        partTwo input `shouldBe` 281
