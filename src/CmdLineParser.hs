{-# LANGUAGE OverloadedStrings #-}

module CmdLineParser where

import Data.Set.Ordered
import Data.Text
import Data.Attoparsec.Text
import Control.Applicative

import Protocol
import Cards

makeMsg :: Text -> Either String Msg
makeMsg input = parseOnly msgParser input

rankParser :: Parser Rank
rankParser = (char '7' >> return Seven)
         <|> (char '8' >> return Eight)
         <|> (char '9' >> return Nine)
         <|> (string "10" >> return Ten)
         <|> (char 'J' >> return Jack)
         <|> (char 'Q' >> return Queen)
         <|> (char 'K' >> return King)
         <|> (char 'A' >> return Ace)

suitParser :: Parser Suit
suitParser = (char 'd' >> return Diamonds)
         <|> (char 'h' >> return Hearts)
         <|> (char 'c' >> return Clubs)
         <|> (char 's' >> return Spades)

cardParser :: Parser Card
cardParser = do
  rank <- rankParser
  suit <- suitParser
  return $ Card rank suit

handParser :: Parser Hand
handParser = do
  char '('
  cards <- many' $ skipMany (char ',') >> cardParser
  char ')'
  return $ fromList cards

msgExchangeParser :: Parser Msg
msgExchangeParser = do
  string "e "
  hand <- handParser
  return $ Exchange hand

msgDeclareCombinationParser :: Parser Msg
msgDeclareCombinationParser = do
  string "d "
  hand <- handParser
  return $ DeclareCombination hand

msgChangeName :: Parser Msg
msgChangeName = do
  name <- takeText
  return $ ChangeName name

msgParser :: Parser Msg
msgParser = msgDeclareCombinationParser
        <|> msgExchangeParser
        -- <|> msgChangeName
