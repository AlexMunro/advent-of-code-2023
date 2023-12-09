{-# LANGUAGE FlexibleContexts #-}

module Day07 (partOne, partTwo) where

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
                       , valuation :: Char -> Int
                       , cardTally :: [Char] -> [Int]
                       }

instance Eq HandBid where
  hb1 == hb2 = (hand hb1) == (hand hb2)

instance Ord HandBid where
  compare hb1 hb2
    | handTypeRank hb1 > handTypeRank hb2 = GT
    | handTypeRank hb1 < handTypeRank hb2 = LT
    | otherwise                                         = tiebreaker (valuation hb1) (hand hb1) (hand hb2)

parseHandBid :: Parsec.Parsec String () HandBid
parseHandBid = do
  hand <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
  Parsec.char ' '
  bid <- read <$> Parsec.many1 Parsec.digit
  return $ HandBid hand bid charToCardVal cardTallies

parseAllHandBids :: [String] -> Either Parsec.ParseError [HandBid]
parseAllHandBids input = sequence $ parse parseHandBid <$> input 

charToCardVal :: Char -> Int
charToCardVal 'A' = 14
charToCardVal 'K' = 13
charToCardVal 'Q' = 12
charToCardVal 'J' = 11
charToCardVal 'T' = 10
charToCardVal n = digitToInt n

jokerCharToCardVal :: Char -> Int
jokerCharToCardVal 'J' = 1
jokerCharToCardVal n = charToCardVal n

tiebreaker :: (Char -> Int) -> [Char] -> [Char] -> Ordering
tiebreaker _ [] [] = EQ
tiebreaker cardVal (x:xs) (y:ys)
  | (cardVal x) > (cardVal y) = GT
  | (cardVal x) < (cardVal y) = LT
  | otherwise = tiebreaker cardVal xs ys

type HandTypeCheck = HandBid -> Bool

handTypeRank :: HandBid -> Int
handTypeRank hb
  | fiveOfAKind hb  = 6
  | fourOfAKind hb  = 5
  | fullHouse hb    = 4
  | threeOfAKind hb = 3
  | twoPair hb      = 2
  | onePair hb      = 1
  | otherwise       = 0

cardTallies :: [Char] -> [Int]
cardTallies hand = sort . map length . group $ sort hand

jokerCardTallies :: [Char] -> [Int]
jokerCardTallies ['J', 'J', 'J', 'J', 'J'] = [5] -- You'll never guess which edge case I hit!
jokerCardTallies hand = init regularTallies ++ [last regularTallies + jokerCount]
  where regularTallies = cardTallies handWithoutJokers
        handWithoutJokers = filter (/= 'J') hand
        jokerCount = length hand - length handWithoutJokers

talliedCards :: HandBid -> [Int]
talliedCards hb = (cardTally hb) (hand hb)

fiveOfAKind :: HandTypeCheck
fiveOfAKind hb = talliedCards hb == [5]

fourOfAKind :: HandTypeCheck
fourOfAKind hb = talliedCards hb == [1, 4]

fullHouse :: HandTypeCheck
fullHouse hb = talliedCards hb  == [2, 3]

threeOfAKind :: HandTypeCheck
threeOfAKind hb = talliedCards hb == [1, 1, 3]

twoPair :: HandTypeCheck
twoPair hb = talliedCards hb == [1, 2, 2]

onePair :: HandTypeCheck
onePair hb = talliedCards hb == [1, 1, 1, 2]

calculateWinnings :: [HandBid] -> [Int]
calculateWinnings handBids = map winnings rankedHands
  where rankedHands = zip [1..] $ sort handBids
        winnings (rank, hb) = rank * (bid hb)

standardToJoker :: HandBid -> HandBid
standardToJoker (HandBid h b _ _ ) = HandBid h b jokerCharToCardVal jokerCardTallies

partOne :: [String] -> Int
partOne input = do
  case parseAllHandBids input of
    Right handBids -> sum $ calculateWinnings handBids
    Left err -> -1

partTwo :: [String] -> Int
partTwo input = do
  case parseAllHandBids input of
    Right handBids -> sum $ calculateWinnings $ map standardToJoker handBids
    Left err -> -1
