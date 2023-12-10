import Test.Hspec
import Control.Exception
import Day08

main :: IO ()
main = hspec $ do
  describe "Day08" $ do
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
                            

    describe "partOne" $ do
      it "returns the steps required to reach ZZZ" $ do
        partOne input `shouldBe` 2

      it "handles the instruction list repeating" $ do
        partOne challengingInput `shouldBe` 6
