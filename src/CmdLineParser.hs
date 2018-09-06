{-# LANGUAGE OverloadedStrings #-}

module CmdLineParser where

import Data.Set.Ordered
import Data.Text
import Data.Void
import Control.Applicative hiding (many, some)
import Control.Arrow (left)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char

import Protocol
import Cards

-- Void : no custom error messages
-- Text : input data type
type Parser = Parsec Void Text

makeMsg :: Text -> Either String Msg
makeMsg input = left parseErrorPretty $ parse msgParser "" input

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
  cards <- many $ skipMany (char ',') >> cardParser
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
  name <- takeRest
  return $ ChangeName name

msgParser :: Parser Msg
msgParser = msgDeclareCombinationParser
        <|> msgExchangeParser
        -- <|> msgChangeName
