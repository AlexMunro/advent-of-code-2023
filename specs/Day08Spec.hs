import Test.Hspec
import Control.Exception
import Day08

main :: IO ()
main = hspec $ do
  describe "Day08" $ do
    describe "partOne" $ do
      let input = ["RL",
                    "",
                    "AAA = (BBB, CCC)",
                    "BBB = (DDD, EEE)",
                    "CCC = (ZZZ, GGG)",
                    "DDD = (DDD, DDD)",
                    "EEE = (EEE, EEE)",
                    "GGG = (GGG, GGG)",
                    "ZZZ = (ZZZ, ZZZ)"]

      let challengingInput = ["LLR",
                              "",
                              "AAA = (BBB, BBB)",
                              "BBB = (AAA, ZZZ)",
                              "ZZZ = (ZZZ, ZZZ)"]
                            
      it "returns the steps required to reach ZZZ" $ do
        partOne input `shouldBe` 2

      it "handles the instruction list repeating" $ do
        partOne challengingInput `shouldBe` 6

    describe "partTwo" $ do
      let input = ["LR",
                   "",
                   "11A = (11B, XXX)",
                   "11B = (XXX, 11Z)",
                   "11Z = (11B, XXX)",
                   "22A = (22B, XXX)",
                   "22B = (22C, 22C)",
                   "22C = (22Z, 22Z)",
                   "22Z = (22B, 22B)",
                   "XXX = (XXX, XXX)"]

      it "returns the steps required to exclusively reach **Z locations" $ do
        partTwo input `shouldBe` 6
