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

import PiquetTypes hiding (PlayerMove(..))
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
         <|> (char' 'J' >> return Jack) -- char' = case insensitive
         <|> (char' 'Q' >> return Queen)
         <|> (char' 'K' >> return King)
         <|> (char' 'A' >> return Ace)

suitParser :: Parser Suit
suitParser = (char' 'd' >> return Diamonds)
         <|> (char' 'h' >> return Hearts)
         <|> (char' 'c' >> return Clubs)
         <|> (char' 's' >> return Spades)

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
msgExchangeParser = string "e " >> (Exchange <$> handParser)

msgDeclareCombinationParser :: Parser Msg
msgDeclareCombinationParser = string "d " >> (DeclareCombination <$> handParser)

msgDeclareCarteBlancheParser :: Parser Msg
msgDeclareCarteBlancheParser = string "d cb" >> return DeclareCarteBlanche

msgDeclareCarteRougeParser :: Parser Msg
msgDeclareCarteRougeParser = string "d cr" >> return DeclareCarteRouge

msgDeclarationResponseParser :: Parser Msg
msgDeclarationResponseParser = 
      (string "r good"     >> (return $ Respond Good))
  <|> (string "r not good" >> (return $ Respond NotGood))
  <|> (string "r equals"   >> (return $ Respond Equals))

msgPlayCardParser :: Parser Msg
msgPlayCardParser = string "p " >> PlayCard <$> cardParser

msgChangeName :: Parser Msg
msgChangeName = ChangeName <$> takeRest

msgParser :: Parser Msg
msgParser = msgDeclareCarteBlancheParser
        <|> msgExchangeParser
        <|> msgDeclareCombinationParser
        <|> msgDeclarationResponseParser
        <|> msgDeclareCarteRougeParser
        <|> msgPlayCardParser
        -- <|> msgChangeName
