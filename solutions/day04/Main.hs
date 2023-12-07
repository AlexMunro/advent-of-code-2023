module Main where
import Day04

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day04.txt"
  putStrLn $ show $ partOne input
