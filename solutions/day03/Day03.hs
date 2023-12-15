module Day03 (partOne) where

import Data.Char
import qualified Data.Set as Set

data IdentifiedNumber = IdentifiedNumber { value :: Int
                                         , line :: Int
                                         , start :: Int
                                         , end :: Int } deriving (Show)

symbolsFromLine :: ([Char], Int) -> Set.Set (Int, Int)
symbolsFromLine (line, lineNumber) = Set.fromList $ formatForSet <$> filter validSymbol (zip line [0..])
  where validSymbol (c, _) = not (c == '.' || isDigit c || isAlpha c)
        formatForSet (_, idx) = (idx, lineNumber)

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
  
parseInput :: [String] -> (Set.Set (Int, Int), [IdentifiedNumber])
parseInput input = (symbolsFromLines, numbersFromLines)
  where symbolsFromLines = Set.unions $ map symbolsFromLine zippedInput
        numbersFromLines = foldr (++) [] (map numbersFromLine zippedInput)
        zippedInput = zip input [0..]

isAdjacentToSymbol :: Set.Set(Int, Int) -> IdentifiedNumber -> Bool
isAdjacentToSymbol symbolPoints number = any isSymbol [(x, y) | x <- xRange, y <- yRange]
  where isSymbol loc = Set.member loc symbolPoints
        xRange = [(start number -1)..(end number + 1)]
        yRange = [(line number - 1)..(line number + 1)]
        
partNumbers :: [String] -> [IdentifiedNumber]
partNumbers input = filter (isAdjacentToSymbol symbolPoints) numbers
  where (symbolPoints, numbers) = parseInput input
  

partOne :: [String] -> Int
partOne input = sum $ value <$> partNumbers input 

