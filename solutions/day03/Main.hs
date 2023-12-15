module Main where
import Day03

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day03.txt"
  putStrLn $ show $ partOne input
