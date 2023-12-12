module Main where
import Day02

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day02.txt"
  putStrLn $ show $ partOne input
