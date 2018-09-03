{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import Data.Text (Text)
import Data.Aeson hiding ((.=))
import Data.Aeson.Casing
import Data.Binary hiding (get)
import GHC.Generics


import Network.GameEngine
import Cards
import Game

data Msg = SetCombination Hand 
         | ChangeName Text 
         deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)


data ViewGame = ViewGame { viewGame :: Step
                 , viewSampleCommands :: [Msg]
                 -- } deriving (Show, Eq, Binary, Generic, ToJSON, FromJSON)
                 } deriving (Show, Eq, Binary, Generic)

data ViewPlayer = ViewPlayer { playerSendPortId :: String 
                             , playerName :: String
                             , playerHand :: Hand
                             } deriving (Show, Eq, Binary, Generic)
                             -- } deriving (Show, Eq, Binary, Generic, ToJSON, FromJSON)

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

type GameStateMsg = (ViewGame, [ViewPlayer])
