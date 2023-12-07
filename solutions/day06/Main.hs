module Main where
import Day06

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day06.txt"
  putStrLn $ show $ partOne input
  putStrLn $ show $ partTwo input
