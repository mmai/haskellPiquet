{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.ByteString hiding (putStrLn, pack)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text           (Text, pack)
import           Data.Text.Encoding  (decodeUtf8)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Aeson hiding ((.=))
import           Data.Aeson.Text
import qualified Network.WebSockets  as WS

import Protocol

--------------------------------------------------------------------
-- Base WebSocket helpers

-- receiveJSON :: (FromJSON j, MonadIO m) => WS.Connection -> m (Either String j)
-- receiveJSON = liftIO . fmap eitherDecode . WS.receiveData
--
-- sendJSON :: (ToJSON j, MonadIO m) => WS.Connection -> j -> m ()
-- sendJSON conn = liftIO . WS.sendTextData conn . encode
--------------------------------------------------------------------

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn $ either pack displayState $ eitherDecode msg

    -- Read from stdin and write to WS
    let loop = do
          line <- T.getLine
          unless (T.null line) $ WS.sendTextData conn ( encode $ makeMsg line)
          loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

makeMsg :: Text -> Msg
makeMsg input = ChangeName input

displayState :: GameStateMsg -> Text
displayState (game, player:_) = pack infos where
  infos =  "Game State: " <> show (viewGame game)
        <> "\nYou play has " <> playerName player
        <> if (show (viewGame game) == "Start")
              then "\nWaiting for an opponent to connect..."
              else 
                "\nYour hand is " <> show (playerHand player) <>
                if playerIsActive player 
                        then "\nWaiting for your move... "
                        else "\nWaiting for your opponent to play..." 

--------------------------------------------------------------------------------
main :: IO ()
-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app
main = withSocketsDo $ WS.runClient "localhost" 8000 "/" app
