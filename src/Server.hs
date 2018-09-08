{-# LANGUAGE TupleSections #-}

module Server where

import Control.Distributed.Process
import Control.Lens
import Control.Arrow (left)
import Data.Aeson
import Data.Aeson.Casing
import Data.Binary
import Data.Maybe
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.GameEngine

import PiquetTypes hiding (PlayerMove(..))
import Protocol
import Game

update :: EngineMsg Msg -> Game -> Either (PiquetError, String) Game
update (GameMsg playerId msg) g = handlePlayerMsg msg playerId g
update (Join playerId) g
  | isNothing (g ^. player1SendPortId) = Right $ g & player1SendPortId ?~ playerId -- ?~ == .~ Just
  | isNothing (g ^. player2SendPortId) = Right $ g & player2SendPortId ?~ playerId
                                              & chooseElder
                                              & deal
  | otherwise                          = Left (AlreadyConnectedError, show playerId)
update (Leave playerId) g
  | g ^. player1SendPortId == Just playerId = Right $ g & player1SendPortId .~ Nothing
  | g ^. player2SendPortId == Just playerId = Right $ g & player2SendPortId .~ Nothing
  | otherwise                               = Left (NotConnectedError, show playerId)

handlePlayerMsg :: Msg -> SendPortId -> Game -> Either (PiquetError, String) Game
handlePlayerMsg DeclareCarteBlanche spid = (left (, show spid)) . (checkCarteBlanche (getPortIdPlayerLens spid))
handlePlayerMsg (Exchange hand)     spid = Right . changePlayerCards hand (getPortIdPlayerLens spid)
handlePlayerMsg (ChangeName name')  spid = Right . ((getPortIdPlayerLens spid . name) .~ unpack name')
handlePlayerMsg _                   spid = const $ Left (UnknownCommand, show spid)

viewG :: Either (PiquetError, String) Game -> GameStateMsg
viewG (Left err) = Left err
viewG (Right g) = Right ( ViewGame { viewGame = g ^. step
                     , viewSampleCommands = [ChangeName (pack "Kris")]
                     }
          , [ ViewPlayer { playerName       = g ^. player1 . name
                         , playerPoints     = g ^. player1 . dealPoints
                         , playerHand       = g ^. player1 . hand
                         , playerIsActive   = player1IsActive
                         , playerSendPortId = maybe "" show (g ^. player1SendPortId)
                         }
            , ViewPlayer { playerName       = g ^. player2 . name
                         , playerPoints     = g ^. player2 . dealPoints
                         , playerHand       = g ^. player2 . hand
                         , playerIsActive   = not player1IsActive
                         , playerSendPortId = maybe "" show (g ^. player2SendPortId)
                         }
            ]
          ) where
            player1IsActive = g ^. isElderToPlay == g ^. player1 . isElder
