module Main where
import Day01

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day01.txt"
  putStrLn (show (partOne input))
  putStrLn (show (partTwo input))

