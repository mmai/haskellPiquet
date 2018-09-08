{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Casing
import Data.Binary
import Control.Distributed.Process (SendPortId)
import GHC.Generics


import Network.GameEngine
import Cards
import PiquetTypes

data Msg = Exchange Hand 
         | DeclareCarteBlanche
         | DeclareCarteRouge
         | DeclareCombination Hand 
         | Respond DeclarationResponse 
         | PlayCard Card 
         | ChangeName Text 
         deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)


data ViewGame = ViewGame { viewGame :: Step
                 , viewSampleCommands :: [Msg]
                 } deriving (Show, Eq, Binary, Generic)

data ViewPlayer = ViewPlayer { playerSendPortId :: String 
                             , playerName :: String
                             , playerPoints :: Int
                             , playerHand :: Hand
                             , playerIsActive :: Bool
                             } deriving (Show, Eq, Binary, Generic)

instance TaggedView ViewPlayer where
  destinationPortId = playerSendPortId

type View = (ViewGame, [ViewPlayer])

instance ToJSON ViewGame where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ViewGame where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToJSON ViewPlayer where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ViewPlayer where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

type GameStateMsg = Either (PiquetError, String) (ViewGame, [ViewPlayer])
-- type GameStateMsg = Either (PiquetError, SendPortId) (ViewGame, [ViewPlayer])
