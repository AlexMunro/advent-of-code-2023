{-# LANGUAGE FlexibleContexts #-}

module Day07 (partOne) where

import Data.Char
import Data.List
import Data.Ord

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text

data HandBid = HandBid { hand :: [Char]
                       , bid :: Int
		       } deriving (Show, Eq)

instance Ord HandBid where
  compare hb1 hb2
    | handTypeRank (hand hb1) > handTypeRank (hand hb2) = GT
    | handTypeRank (hand hb1) < handTypeRank (hand hb2) = LT
    | otherwise                                         = tiebreaker (hand hb1) (hand hb2)

parseHandBid :: Parsec.Parsec String () HandBid
parseHandBid = do
  hand <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
  Parsec.char ' '
  bid <- read <$> Parsec.many1 Parsec.digit
  return $ HandBid hand bid

parseAllHandBids :: [String] -> Either Parsec.ParseError [HandBid]
parseAllHandBids input = sequence $ parse parseHandBid <$> input 

charToCardVal :: Char -> Int
charToCardVal 'A' = 14
charToCardVal 'K' = 13
charToCardVal 'Q' = 12
charToCardVal 'J' = 11
charToCardVal 'T' = 10
charToCardVal n = digitToInt n

tiebreaker :: [Char] -> [Char] -> Ordering
tiebreaker [] [] = EQ
tiebreaker (x:xs) (y:ys)
  | (charToCardVal x) > (charToCardVal y) = GT
  | (charToCardVal x) < (charToCardVal y) = LT
  | otherwise = tiebreaker xs ys

type HandTypeCheck = [Char] -> Bool

handTypeRank :: [Char] -> Int
handTypeRank hand
  | fiveOfAKind hand  = 6
  | fourOfAKind hand  = 5
  | fullHouse hand    = 4
  | threeOfAKind hand = 3
  | twoPair hand      = 2
  | onePair hand      = 1
  | otherwise         = 0

cardTallies :: [Char] -> [Int]
cardTallies hand = sort . map length . group $ sort hand

fiveOfAKind :: HandTypeCheck
fiveOfAKind hand = cardTallies hand == [5]

fourOfAKind :: HandTypeCheck
fourOfAKind hand = cardTallies hand == [1, 4]

fullHouse :: HandTypeCheck
fullHouse hand = cardTallies hand == [2, 3]

threeOfAKind :: HandTypeCheck
threeOfAKind hand = cardTallies hand == [1, 1, 3]

twoPair :: HandTypeCheck
twoPair hand = cardTallies hand == [1, 2, 2]

onePair :: HandTypeCheck
onePair hand = cardTallies hand == [1, 1, 1, 2]

calculateWinnings :: [HandBid] -> [Int]
calculateWinnings handBids = map winnings rankedHands
  where rankedHands = zip [1..] $ sort handBids
        winnings (rank, hb) = rank * (bid hb)

partOne :: [String] -> Int
partOne input = do
  case parseAllHandBids input of
    Right handBids -> sum $ calculateWinnings handBids
    Left err -> -1
