import Test.Hspec
import Control.Exception
import Day03

main :: IO ()
main = hspec $ do
 describe "Day03" $ do
    let input = ["467..114..",
                 "...*......",
                 "..35..633.",
                 "......#...",
                 "617*......",
                 ".....+.58.",
                 "..592.....",
                 "......755.",
                 "...$.*....",
                 ".664.598.."] 

    describe "partOne" $ do
      it "sums all of the part numbers" $ do
        partOne input `shouldBe` 4361

    describe "partTwo" $ do
      it "multiplies the two gear numbers" $ do
        partTwo input `shouldBe` 467835


