module Main where
import Day08

main :: IO ()
main = do
  input <- filter (\x -> (length x) > 0) . lines <$> readFile "inputs/day08.txt"
  putStrLn $ show $ partOne input
 
