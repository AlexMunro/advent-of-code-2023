module Main where
import Day07

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day07.txt"
  putStrLn $ show $ partOne input
