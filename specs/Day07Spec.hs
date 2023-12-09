import Test.Hspec
import Control.Exception
import Day07

main :: IO ()
main = hspec $ do
  describe "Day07" $ do
    let input = ["32T3K 765",
                 "T55J5 684",
                 "KK677 28",
                 "KTJJT 220",
                 "QQQJA 483"]

    describe "partOne" $ do
      it "returns the total winnings" $ do
        partOne input `shouldBe` 6440

    describe "partTwo" $ do
      it "returns the total winnings with the joker rule" $ do
        partTwo input `shouldBe` 5905
