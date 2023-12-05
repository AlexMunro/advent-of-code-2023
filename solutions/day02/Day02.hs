{-# LANGUAGE FlexibleContexts #-}

module Day02 (partOne, partTwo) where

-- with thanks to https://jsdw.me/posts/haskell-parsec-basics/

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text

data Colour = Red | Green | Blue deriving (Show, Enum, Eq) 

data Game = Game { id :: Int
                 , ballCounts :: [BallCount]
                 } deriving (Show) 

data BallCount = BallCount { count :: Int
                           , colour :: Colour 
                           } deriving (Show)

parseColour :: Parsec.Parsec String () Colour
parseColour = parseRed <|> parseGreen <|> parseBlue
  where
    parseRed = Parsec.string "red" >> return Red
    parseGreen = Parsec.string "green" >> return Green
    parseBlue = Parsec.string "blue" >> return Blue

parseGame :: Parsec.Parsec String () Game
parseGame = do
  Parsec.string "Game "
  id <- read <$> Parsec.many1 Parsec.digit
  Parsec.string ": "  
  ballCounts <- Parsec.sepBy parseBallCount parseSeparator 
  return $ Game id ballCounts

parseBallCount :: Parsec.Parsec String () BallCount
parseBallCount = do
  count <- read <$> Parsec.many1 Parsec.digit
  Parsec.string " "
  colour <- parseColour
  return $ BallCount count colour

parseSeparator :: Parsec.Parsec String () ()
parseSeparator = (Parsec.char ',' <|> Parsec.char ';') >> Parsec.spaces

parseAllGames :: [String] -> Either Parsec.ParseError [Game]
parseAllGames gameListings = sequence $ parse parseGame <$> gameListings

-- with thanks to https://www.haskellsos.com/basics/how-are-type-errors-handled-in-haskells-either-monad/
handleParseError :: String -> IO ()
handleParseError err = putStrLn ("Error: " ++ err)

maxOfColour :: Game -> Colour -> Int
maxOfColour game selectColour = maximum ballCountsOfSelectColour
  where ballCountsOfSelectColour = count <$> filter (\bc -> colour bc == selectColour) (ballCounts game)

validBallCounts :: Game -> Bool
validBallCounts game = maxOfColour game Red <= 12 && maxOfColour game Green <= 13 && maxOfColour game Blue <= 14  

minSet :: Game -> [Int]
minSet game = [maxOfColour game Red, maxOfColour game Green, maxOfColour game Blue]

partOne :: [String] -> Int
partOne input = do
  case parseAllGames input of
    Right games -> sum $ Day02.id <$> filter validBallCounts games
    Left err -> -1

partTwo :: [String] -> Int
partTwo input = do
  case parseAllGames input of
    Right games -> sum $ product <$> minSet <$> games
    Left err -> -1
