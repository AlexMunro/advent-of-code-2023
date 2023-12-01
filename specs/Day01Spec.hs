import Test.Hspec
import Control.Exception
import Day01

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    it "gives you a very small number" $ do
      partOne `shouldBe` 5

