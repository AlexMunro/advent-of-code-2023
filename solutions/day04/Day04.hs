
{-# LANGUAGE FlexibleContexts #-}

module Day04 (partOne, partTwo) where

import Data.List
import qualified Data.Map as Map

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text

data Card = Card { cardNo :: Int
                 , winningNumbers :: [Int]
                 , myNumbers :: [Int]
                 } deriving (Show) 

parseNumbers :: Parsec.Parsec String () [Int]
parseNumbers = Parsec.sepEndBy1 (read <$> Parsec.many1 Parsec.digit) Parsec.spaces

parseCard :: Parsec.Parsec String () Card
parseCard = do
  Parsec.string "Card"
  Parsec.spaces
  cardNo <- read <$> Parsec.many1 Parsec.digit
  Parsec.char ':'
  Parsec.spaces -- this one is weirdly only required for the example cases, but not the real inputs
  winningNumbers <- parseNumbers
  Parsec.char '|'
  Parsec.spaces
  myNumbers <- parseNumbers
  return $ Card cardNo winningNumbers myNumbers

parseAllCards :: [String] -> Either Parsec.ParseError [Card]
parseAllCards cardListings = sequence $ parse parseCard <$> cardListings

correctlySelectedNumbers :: Card -> Int
correctlySelectedNumbers card = length $ (winningNumbers card) `intersect` (myNumbers card)

score :: Card -> Int
score card
  | correctlySelectedNumberCount > 0 = 2^(correctlySelectedNumberCount -1)
  | otherwise = 0
  where correctlySelectedNumberCount = correctlySelectedNumbers card

cardMap :: [Card] -> Map.Map Int Card
cardMap cards = Map.fromList $ map (\card -> ((cardNo card), card)) cards

resultingCards :: Int -> Map.Map Int Card -> [Card]
resultingCards cardNo cardMap = case Map.lookup cardNo cardMap ouf
  Just card -> card : concat [resultingCards n cardMap | n <- [cardNo+1..cardNo + (correctlySelectedNumbers   Nothing -> []

totalResultingCards :: [Card] -> [Card]
totalResultingCards cards = concat $ map (\n -> resultingCards n (cardMap cards)) initialCorrectNumbers
  where initialCorrectNumbers = map cardNo cards

partOne :: [String] -> Int
partOne input = do
  case parseAllCards input of
    Right cards -> sum $ score <$> cards
    Left err -> -1

partTwo :: [String] -> Int
partTwo input = do
  case parseAllCards input of
    Right cards -> length $ totalResultingCards cards
    Left err -> -1
