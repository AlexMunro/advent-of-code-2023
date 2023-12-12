module Main where
import Day08

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day08.txt"
  putStrLn $ show $ partOne input
  putStrLn $ show $ partTwo input
 
