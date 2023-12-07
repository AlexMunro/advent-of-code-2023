{-# LANGUAGE FlexibleContexts #-}

module Day06 (partOne, partTwo) where

import Data.Char

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text

parseRecordLine :: String -> Parsec.Parsec String () [Int]
parseRecordLine title = do
  Parsec.string title
  Parsec.char ':'
  Parsec.spaces
  records <- Parsec.sepBy (read <$> Parsec.many1 Parsec.digit) Parsec.spaces
  return records

parseAllLines :: [String] -> Either Parsec.ParseError [(Int, Int)]
parseAllLines lines = do
  times <- parse (parseRecordLine "Time") (lines !! 0)
  distances <- parse (parseRecordLine "Distance") (lines !! 1)
  return $ zip times distances

raceDistance :: Int -> Int -> Int
raceDistance holdTime totalTime = (totalTime - holdTime) * holdTime

potentialImprovements :: (Int, Int) -> Int
potentialImprovements (time, distance) = length [t | t <- [1.. time - 1], (raceDistance t time) > distance]

parseMegaRecord :: [String] -> (Int, Int)
parseMegaRecord lines = (nums !! 0, nums !! 1)
  where nums = (read :: String -> Int) <$> map (filter Data.Char.isDigit) lines 

partOne :: [String] -> Int
partOne input = do
  case parseAllLines input of
    Right records -> product $ potentialImprovements <$> records
    Left err -> -1

partTwo :: [String] -> Int
partTwo input = potentialImprovements $ parseMegaRecord input
