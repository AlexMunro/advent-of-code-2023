import Test.Hspec
import Control.Exception
import Day02

main :: IO ()
main = hspec $ do
  describe "Day02" $ do
    let input = [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"] 

    describe "partOne" $ do
      it "sums the IDs of games that can be played with 12 red, 13 green and 14 blue cubes" $ do
        partOne input `shouldBe` 8 
    
    describe "partTwo" $ do
      it "sums the powers of minimal sets of cubes for each game" $ do
        partTwo input `shouldBe` 2286 
