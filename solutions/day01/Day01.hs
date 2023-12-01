module Day01 (partOne, calibrationValue) where

import Data.Char

partOne :: [String] -> Integer
partOne input = sum (map calibrationValue input) 

calibrationValue :: String -> Integer
calibrationValue line = read([firstDigit] ++ [lastDigit])
  where firstDigit = head $ filter isDigit line 
        lastDigit = head $ reverse $ filter isDigit line

