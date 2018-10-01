{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.GameEngine where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets -- Message 
import Reflex
import Reflex.Base
import Reflex.WebSocketServer (sockets, runReflexBase')
import Servant

-- Endpoints : /socket or 
type MyServer = "socket" :> Raw 
           :<|> Raw

runGame
  :: ( Serializable msg
     , Serializable gameView
     , Serializable playerView
     , FromJSON msg
     , ToJSON gameView
     , ToJSON playerView
     , Show gameView
     , Show playerView
     , Show state
     , Show msg
     , Show err
     , Binary err
     , ToJSON err
     , Serializable err
     , TaggedView playerView
     )
  => state 
  -> ( EngineMsg msg -> state -> SocketMsgGame err state ) 
  -> ( SocketMsgGame err state -> SocketMsgView err gameView playerView ) 
  -> IO ()
runGame initialGameState update view = do
  putStrLn "Starting server on port 8080"
  -- Build the websocket handler and get back the threads that need killing
  (serverApp, _, eventLoopThread) <- runReflexBase' (app initialGameState update view)
  let socketServer = websocketsOr defaultConnectionOptions serverApp $ \_ respond ->
        respond $ responseLBS status400 [] "Not a WebSocket request"
      mainServer = socketServer :<|> serveDirectory "./static"
  -- Serve the websocket handler and static assets.
  -- Make sure we kill those threads (this is important in GHCi)
  -- TODO: Figure out how to use ResourceT to kill the threads?
  finally (run 8080 $ serve @MyServer Proxy mainServer) $ killThread eventLoopThread

app
  :: ( MonadFix m
     , MonadHold t m
     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     , TriggerEvent t m

     , Serializable msg
     , Serializable gameView
     , Serializable playerView
     , FromJSON msg
     , ToJSON gameView
     , ToJSON playerView
     , Show gameView
     , Show playerView
     , Show state
     , Show msg
     , Show err
     , Binary err
     , ToJSON err
     , Serializable err
     , TaggedView playerView
     )
  => state 
  -> ( EngineMsg msg -> state -> SocketMsgGame err state ) 
  -> ( SocketMsgGame err state -> SocketMsgView err gameView playerView ) 
  -> m ServerApp
app initialGameState update view =
  -- Create a websocket handler
  sockets $ \incomingMessages -> do
    -- Keep all connections in a map.
    -- Set values to the last received message.
    -- Delete connections that disconnect.
    messageState <- foldDyn (uncurry processMessage) Map.empty incomingMessages
    let outgoingMessages = tag (current messageState) tick --tick : from deleted trigger
    -- Every second, send each connection the last message they sent us and print it
    performEvent_ $ fmap (liftIO . print) incomingMessages
    -- return (triggerThread, outgoingMessages)
    return outgoingMessages

processMessage :: ConnectionId -> Message -> Map ConnectionId Message -> Map ConnectionId Message
processMessage connId (ControlMessage (Close _ _)) messages = Map.delete connId messages
processMessage connId message messages
  | connId `notElem` messages = 
  
  Map.insert connId message
