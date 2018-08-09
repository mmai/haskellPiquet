module Game
    ( play
    ) where

import Cards
import Shuffle
import Control.Monad.State
import Rainbow

play :: IO ()
play = do
  deck <- shuffleIO sortedDeck
  let (hands, tally) = drawHands deck 12 2 
  print hands
  print tally

data Role = Elder | Younger

data Player = Player { hand :: Hand
                     , roundPoints :: Int
                     , gamePoints :: Int
                     , points :: Int
                     , name :: String
                     , role :: Role
                     } deriving (Show)

data Game = Game { deck :: Deck
                 , step :: Step
                 , player1 :: Player
                 , player2 :: Player
                 }

data Step = Start 
          | Cut 
          | Deal
          | ExchangeElder
          | ExchangeYounger 
          | DeclarePointElder
          | DeclarePointResponse 
          | DeclareSequenceElder
          | DeclareSequenceResponse
          | DeclareSetElder
          | DeclareSetResponse
          | DeclareCarteRougeElder
          | Play1
          | DeclarePointYounger
          | DeclareSequenceYounger
          | DeclareSetYounger
          | DeclareCarteRougeYounger
          | Play

