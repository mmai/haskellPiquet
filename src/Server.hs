{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Control.Distributed.Process
import Control.Lens
import Data.Maybe
import Data.Aeson hiding ((.=))
import Data.Aeson.Casing
import Data.Binary hiding (get)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.GameEngine

import Game
import Protocol

data ViewGame = ViewGame { viewGame :: Step
                 , viewSampleCommands :: [Msg]
                 } deriving (Show, Eq, Binary, Generic)

data ViewPlayer = ViewPlayer { playerName :: String 
                             , playerSendPortId :: String
                             } deriving (Show, Eq, Binary, Generic)

instance TaggedView ViewPlayer where
  destinationPortId = playerSendPortId

type View = (ViewGame, (ViewPlayer, ViewPlayer))
-- deriving (Show, Eq, Binary, Generic)

instance ToJSON ViewGame where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON ViewPlayer where
  toJSON = genericToJSON $ aesonPrefix camelCase

update :: EngineMsg Msg -> Game -> Game
update (Join playerId) g
  | isNothing (g ^. player1SendPortId) = g & player1SendPortId .~ Just playerId
  | isNothing (g ^. player2SendPortId) = g & player2SendPortId .~ Just playerId
  | otherwise                          = g
update (Leave playerId) g
  | g ^. player1SendPortId == Just playerId = g & player1SendPortId .~ Nothing
  | g ^. player2SendPortId == Just playerId = g & player2SendPortId .~ Nothing
  | otherwise                               = g
update (GameMsg playerId (ChangeName newName)) g =
  set (getPortIdPlayerLens playerId . name) (unpack newName) g

viewG :: Game -> View
viewG g = ( ViewGame { viewGame = g ^. step
                     , viewSampleCommands = [ChangeName (pack "Kris")]
                     }
          , ( ViewPlayer { playerName       = g ^. player1 . name
                         , playerSendPortId = show (g ^. player1SendPortId)
                         }
            , ViewPlayer { playerName       = g ^. player2 . name
                         , playerSendPortId = show (g ^. player2SendPortId)
                         }
            )
          )
