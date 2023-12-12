{-# LANGUAGE FlexibleContexts #-}

module Day08 (partOne, partTwo) where

import qualified Data.Map as Map
import Data.Maybe

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
parse rule text = Parsec.parse rule "(source)" text

data Instruction = GoLeft | GoRight

data Node = Node { left :: String
                 , right :: String
                 } deriving (Show)

readInstructions :: [Char] -> [Instruction]
readInstructions instrChars = catMaybes $ readInstr <$> instrChars
  where readInstr 'L' = Just GoLeft
        readInstr 'R' = Just GoRight
        readInstr _   = Nothing

parseNodes :: Parsec.Parsec String () (String, Node)
parseNodes = do
  self <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
  Parsec.string " = ("
  left <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
  Parsec.string ", "
  right <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
  Parsec.string ")"
  return $ (self, Node left right)

parseNodeMap :: [String] -> Either Parsec.ParseError (Map.Map String Node)
parseNodeMap input = case parsedMap of
  Right nodeList -> Right $ Map.fromList nodeList
  Left err -> Left err
  where parsedMap = sequence $ parse parseNodes <$> input

navigateNodes :: [Instruction] -> Map.Map String Node -> String -> Int -> Int
navigateNodes _ _ "ZZZ" steps = steps
navigateNodes instructions nodeMap current steps = navigateNodes instructions nodeMap nextLoc (steps + 1)
  where nextLoc = dir currentNode
        currentNode = nodeMap Map.! current
        dir = case instructions !! (steps `mod` length instructions) of
          GoLeft -> left
          GoRight -> right

lcmMulti :: [Int] -> Int
lcmMulti = foldr lcm 1 -- hah, I can't believe this exists in the prelude!

navigateNodesToZ :: [Instruction] -> Map.Map String Node -> String -> Int -> Int
navigateNodesToZ instructions nodeMap current steps
  | last current == 'Z' = steps
  | otherwise = navigateNodesToZ instructions nodeMap nextLoc (steps + 1)
  where nextLoc = dir currentNode
        currentNode = nodeMap Map.! current
        dir = case instructions !! (steps `mod` length instructions) of
          GoLeft -> left
          GoRight -> right

navigateAllNodes :: [Instruction] -> Map.Map String Node -> Int
navigateAllNodes instructions nodeMap = lcmMulti $ map (\node -> navigateNodesToZ instructions nodeMap node 0) startingNodes
  where startingNodes = filter (\s -> last s == 'A') (Map.keys nodeMap)

partOne :: [String] -> Int
partOne input = do
  case parseNodeMap (drop 2 input) of
    Right nodeMap -> navigateNodes (readInstructions $ head input) nodeMap "AAA" 0
    Left err -> -1

partTwo :: [String] -> Int
partTwo input = do
  case parseNodeMap (drop 2 input) of
    Right nodeMap -> navigateAllNodes (readInstructions $ head input) nodeMap
    Left err -> -1
