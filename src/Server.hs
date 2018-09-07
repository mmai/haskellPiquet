module Server where

import Control.Distributed.Process
import Control.Lens
import Data.Maybe
import Data.Aeson
import Data.Aeson.Casing
import Data.Binary
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.GameEngine

import PiquetTypes hiding (PlayerMove(..))
import Protocol
import Game

update :: EngineMsg Msg -> Game -> Game
update (GameMsg playerId msg) g = handlePlayerMsg msg playerId g
update (Join playerId) g
  | isNothing (g ^. player1SendPortId) = g & player1SendPortId .~ Just playerId
  | isNothing (g ^. player2SendPortId) = g & player2SendPortId .~ Just playerId
                                           & chooseElder
                                           & deal
  | otherwise                          = g
update (Leave playerId) g
  | g ^. player1SendPortId == Just playerId = g & player1SendPortId .~ Nothing
  | g ^. player2SendPortId == Just playerId = g & player2SendPortId .~ Nothing
  | otherwise                               = g

handlePlayerMsg :: Msg -> SendPortId -> Game -> Game
handlePlayerMsg DeclareCarteBlanche spid = checkCarteBlanche (getPortIdPlayerLens spid)
handlePlayerMsg (Exchange hand)     spid = changePlayerCards hand (getPortIdPlayerLens spid)
handlePlayerMsg (ChangeName name')  spid = (getPortIdPlayerLens spid . name) .~ (unpack name')

viewG :: Game -> GameStateMsg
viewG g = ( ViewGame { viewGame = g ^. step
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
