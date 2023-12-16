module Day03 (partOne, partTwo) where

import Data.Char
import qualified Data.Map as Map

data IdentifiedNumber = IdentifiedNumber { value :: Int
                                         , line :: Int
                                         , start :: Int
                                         , end :: Int } deriving (Show)

symbolsFromLine :: ([Char], Int) -> Map.Map (Int, Int) Char
symbolsFromLine (line, lineNumber) = Map.fromList $ formatForMap <$> filter validSymbol (zip line [0..])
  where validSymbol (c, _) = not (c == '.' || isDigit c || isAlpha c)
        formatForMap (c, idx) = ((idx, lineNumber), c)

numbersFromLine :: ([Char], Int) -> [IdentifiedNumber]
numbersFromLine (line, lineNo) = numbersRec line 0 "" (-1) []
  where numbersRec :: [Char] -> Int -> [Char] -> Int -> [IdentifiedNumber] -> [IdentifiedNumber]
        numbersRec "" pos currentNumber numStart numbers =
          if length currentNumber > 0 then ((IdentifiedNumber (read currentNumber :: Int) lineNo numStart pos): numbers)
          else numbers
        numbersRec (x: xs) pos currentNumber numStart numbers =
          if isDigit x then numbersRec xs (pos + 1) (currentNumber ++ [x]) (if numStart == -1 then pos else numStart) numbers
          else if length currentNumber > 0
            then let newNum = IdentifiedNumber (read currentNumber :: Int) lineNo numStart (pos - 1)
            in numbersRec xs (pos + 1) "" (-1) (newNum: numbers)
          else numbersRec xs (pos + 1) "" (-1) numbers
  
parseInput :: [String] -> (Map.Map (Int, Int) Char, [IdentifiedNumber])
parseInput input = (symbolsFromLines, numbersFromLines)
  where symbolsFromLines = Map.unions $ map symbolsFromLine zippedInput
        numbersFromLines = foldr (++) [] (map numbersFromLine zippedInput)
        zippedInput = zip input [0..]

isAdjacentToSymbol :: Map.Map (Int, Int) Char -> IdentifiedNumber -> Bool
isAdjacentToSymbol symbolPoints number = any isSymbol [(x, y) | x <- xRange, y <- yRange]
  where isSymbol loc = Map.member loc symbolPoints
        xRange = [(start number -1)..(end number + 1)]
        yRange = [(line number - 1)..(line number + 1)]

isGear :: Map.Map (Int, Int) Char -> IdentifiedNumber -> Bool
isGear symbolPoints number = (length $ filter isGearSym [(x, y) | x <- xRange, y <- yRange]) == 2
  where isGearSym loc = Map.lookup loc symbolPoints == Just '*'
        xRange = [(start number -1)..(end number + 1)]
        yRange = [(line number -1)..(line number + 1)]

partNumbers :: [String] -> [IdentifiedNumber]
partNumbers input = filter (isAdjacentToSymbol symbolPoints) numbers
  where (symbolPoints, numbers) = parseInput input

adjacentNumbers :: (Int, Int) -> [IdentifiedNumber] -> [Int]
adjacentNumbers (x, y) numbers = value <$> filter (isAdjacentToNumber (x, y)) numbers

isAdjacentToNumber :: (Int, Int) -> IdentifiedNumber -> Bool
isAdjacentToNumber (x, y) number = x `elem` [(start number - 1)..(end number + 1)] && y `elem` [(line number - 1)..(line number + 1)]

gearRatios :: [String] -> [Int]
gearRatios input = map product actualGears
  where (symbolPoints, numbers) = parseInput input
        potentialGears = Map.keys $ Map.filter (== '*') symbolPoints
        adjacentToEachGear = map (\g -> adjacentNumbers g numbers) potentialGears
        actualGears = filter (\adj -> length adj == 2) adjacentToEachGear

partOne :: [String] -> Int
partOne input = sum $ value <$> partNumbers input 

partTwo :: [String] -> Int
partTwo input = sum $ gearRatios input

