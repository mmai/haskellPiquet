{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module PiquetTypes where

import Data.Aeson
import Data.Binary
import Data.Set.Ordered hiding (filter, null)
import Control.Lens
import Control.Distributed.Process (SendPortId)
import GHC.Generics
import System.Random (StdGen)
import System.IO (Handle) -- TODO remove
import Cards
import Combinations

data Deal = One | Two | Three | Four | Five | Six deriving (Bounded, Eq, Enum, Show)

data Step = Start 
          | Deal
          | ExchangeElder
          | ExchangeYounger 
          | DeclarePointElder
          | DeclarePointResponse 
          | SetPointsPointElder 
          | DeclareSequenceElder
          | DeclareSequenceResponse
          | SetPointsSequenceElder 
          | DeclareSetElder
          | DeclareSetResponse
          | SetPointsSetElder 
          | PlayFirstCard
          | SetPointsPointYounger 
          | SetPointsSequenceYounger 
          | SetPointsSetYounger 
          | PlayCards
          | PlayEnd
          | End
          deriving (Enum, Show, Ord, Eq, Generic, Binary, ToJSON, FromJSON)

data Move = P1Move PlayerMove | P2Move PlayerMove 
  deriving (Eq, Show, Generic, Binary, ToJSON, FromJSON)

data PlayerMove = Exchange Hand | Declaration Combination | PlayCard Card 
  deriving (Eq, Show, Generic, Binary, ToJSON, FromJSON)

data Player = Player { _hand :: Hand
                     , _isElder :: Bool
                     , _leftUntilCarteRouge :: Hand
                     , _cardPlayed :: Maybe Card
                     , _dealPoints :: Int
                     , _dealWons :: Int
                     , _gamePoints :: Int
                     , _points :: Int
                     , _sockHandle :: Handle --TODO remove
                     , _name :: String
                     } 
makeLenses ''Player

instance Show Player where
  show p = (p ^. name) ++ " : "  ++ show (p ^. dealPoints) ++ " rougeLeft=" ++ (show . size) (p ^. leftUntilCarteRouge) ++ " : "++ show (p ^. hand)

data DeclarationResponse = Good | NotGood | Equals 
          deriving (Eq, Show, Binary, Generic, FromJSON, ToJSON)

data DeclarationWinner = Elder | Younger | Tie | Nobody deriving (Eq, Show)

data Game = Game { _stdGen              :: StdGen
                 , _dealNum             :: Deal
                 , _dealMoves           :: [Move]
                 , _deck                :: Deck
                 , _visible             :: Deck
                 , _step                :: Step
                 , _player1             :: Player
                 , _player2             :: Player
                 , _player1SendPortId   :: Maybe SendPortId
                 , _player2SendPortId   :: Maybe SendPortId
                 , _isElderToPlay       :: Bool
                 , _pointWinner         :: DeclarationWinner
                 , _pointCombination    :: Maybe Combination
                 , _sequenceWinner      :: DeclarationWinner
                 , _sequenceCombination :: Maybe Combination
                 , _setWinner           :: DeclarationWinner
                 , _setCombination      :: Maybe Combination
                 }

makeLenses ''Game

instance Show Game where
  show game = "\n--------------------------------"
         ++ "\nStep : " ++ show (game ^. step)
         ++ "\nDeck : " ++ show (game ^. deck)
         ++ "\nElder : " ++ (if game ^. player1 . isElder then "Player1" else "Player2")
         ++ "\nPoint winner : " ++ show (game ^. pointWinner)
         ++ "\nPlayer1 : " ++ show (game ^. player1)
         ++ "\nPlayer2 : " ++ show (game ^. player2)
         ++ "\n-----------------------------\n"
