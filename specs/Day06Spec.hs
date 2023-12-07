import Test.Hspec
import Control.Exception
import Day06

main :: IO ()
main = hspec $ do
  describe "Day06" $ do
    let input = ["Time:      7  15   30", "Distance:  9  40  200"]

    describe "partOne" $ do
      it "returns the product of ways that each race could be won" $ do
        partOne input `shouldBe` 288

    describe "partTwo" $ do
      it "returns the number of ways that the megarace could be won" $ do
        partTwo input `shouldBe` 71503
