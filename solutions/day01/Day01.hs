module Day01 (partOne, partTwo, calibrationValue, wordyCalibrationValue) where

import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

calibrationValue :: String -> Int
calibrationValue line = (10 * firstDigit) + lastDigit
  where firstDigit = digitToInt $ head $ filter isDigit line 
        lastDigit  = digitToInt $ head $ reverse $ filter isDigit line

firstDigitOrNumberWord :: String -> Int
firstDigitOrNumberWord line
  | line =~ "^one"      = 1
  | line =~ "^two"      = 2
  | line =~ "^three"    = 3
  | line =~ "^four"     = 4
  | line =~ "^five"     = 5
  | line =~ "^six"      = 6
  | line =~ "^seven"    = 7
  | line =~ "^eight"    = 8
  | line =~ "^nine"     = 9
  | isDigit (head line) = digitToInt $ head line
  | otherwise           = firstDigitOrNumberWord $ tail line

lastDigitOrNumberWord :: String -> Int
lastDigitOrNumberWord line
  | line =~ "one$"      = 1
  | line =~ "two$"      = 2
  | line =~ "three$"    = 3
  | line =~ "four$"     = 4
  | line =~ "five$"     = 5
  | line =~ "six$"      = 6
  | line =~ "seven$"    = 7
  | line =~ "eight$"    = 8
  | line =~ "nine$"     = 9
  | isDigit (last line) = digitToInt $ last line
  | otherwise           = lastDigitOrNumberWord $ init line

wordyCalibrationValue :: String -> Int
wordyCalibrationValue line = (10 * firstDigitOrNumberWord line) + lastDigitOrNumberWord line

partOne :: [String] -> Int
partOne input = sum $ map calibrationValue input 

partTwo :: [String] -> Int
partTwo input = sum $ map wordyCalibrationValue input
