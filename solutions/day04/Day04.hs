
{-# LANGUAGE FlexibleContexts #-}

module Day04 (partOne) where

import Data.List

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

score :: Card -> Int
score card
  | correctlySelectedNumbers > 0 = 2^(correctlySelectedNumbers -1)
  | otherwise = 0
  where correctlySelectedNumbers = length $ (winningNumbers card) `intersect` (myNumbers card)

partOne :: [String] -> Int
partOne input = do
  case parseAllCards input of
    Right cards -> sum $ score <$> cards
    Left err -> -1
