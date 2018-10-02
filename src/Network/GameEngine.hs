{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GameEngine where

-- from https://github.com/krisajenkins/Cloud-Haskell-Game/blob/master/server/src/Network/GameEngine.hs

-- import           Control.Distributed.Process
-- import           Control.Distributed.Process.Node
-- import           Control.Distributed.Process.Serializable
import           Control.Concurrent.Chan

import qualified Control.Exception                        as Ex
import           Control.Monad
import           Control.Monad.Logger -- runStdoutLoggingT 
import           Control.Monad.Loops
import           Control.Monad.State                      (evalStateT)
import qualified Control.Monad.State                      as State
import           Control.Monad.Trans.Resource             (MonadResource,
                                                           allocate,
                                                           runResourceT)
import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Aeson                               as Aeson
import           Data.Binary
import           Data.Either
import           Data.Foldable
import           Data.Monoid
import qualified Data.Set                                 as Set
import           Data.Time
import           Formatting                               (sformat, (%))
import qualified Formatting                               as F
import           GHC.Generics

import           Network.Transport.InMemory -- createTransport
import           Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp                 as Warp
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets                       as WS

type PlayerId = Int

data EngineMsg msg = Join PlayerId
                   | Leave PlayerId
                   | GameMsg PlayerId msg
                   deriving (Show, Eq, Binary, Generic)

data PubSubMsg view = Sub PlayerId
                    | Unsub PlayerId
                    deriving (Show, Eq, Binary, Generic)

type SocketMsgView err gameView playerView = (Either (err, String) (gameView, [playerView]))
type SocketMsgGame err state = (Either (err, String) state)

timeBetweenPlayerCommands :: NominalDiffTime
timeBetweenPlayerCommands = 0.1

--------------------------------------------------

class TaggedView v where
  destinationPortId :: v -> String 

------------------------------------------------------------
-- Websocket Server & Wiring.
------------------------------------------------------------
runGame
  :: ( Binary msg
     , Binary gameView
     -- , Serializable gameView
     , Binary playerView
     , FromJSON msg
     , ToJSON gameView
     , ToJSON playerView
     , Show gameView
     , Show playerView
     , Show state
     , Show msg
     , Show err
     , ToJSON err
     , Binary err
     , TaggedView playerView
     )
  => state 
  -> ( EngineMsg msg -> state -> SocketMsgGame err state ) 
  -> ( SocketMsgGame err state -> SocketMsgView err gameView playerView ) 
  -> IO ()
runGame initialGameState update view =
  let settings = Warp.setHost "*" . Warp.setPort 8000 $ Warp.defaultSettings
  in runStdoutLoggingT $ do
       logInfoN "START"
       -- backend <- liftIO createTransport
       -- node <- liftIO $ newLocalNode backend initRemoteTable
       logInfoN $
         sformat
           ("Starting listener with settings: " % F.string % ":" % F.int)
           (show $ Warp.getHost settings)
           (Warp.getPort settings)
       _ <-
         -- liftIO . runProcess node $ do
          do
           -- One channel for each data type to transmit
           (txSubscription, rxSubscription) <- newChan
           (txGameView, rxGameView) <- newChan
           (gameMsgChan, rxGameMsg) <- newChan
           -- _ <- spawnLocal $ broadcaster rxGameView rxSubscription 
           -- _ <- spawnLocal $ gameProcess rxGameMsg txGameView update view initialGameState
           _ <- forkIO $ broadcaster rxGameView rxSubscription 
           _ <- forkIO $ gameProcess rxGameMsg txGameView update view initialGameState
           liftIO . Warp.runSettings settings $
             websocketsOr
               WS.defaultConnectionOptions
               (runResourceT .  acceptClientConnection gameMsgChan txSubscription)
               -- (runResourceT .  acceptClientConnection node txGameMsg txSubscription)
               (staticApp $ defaultFileServerSettings "../client/dist") -- TODO : serveur appli web  ??
       logInfoN "END"

acceptClientConnection
  :: ( MonadResource m
     , Binary msg
     , Binary view
     , FromJSON msg
     , ToJSON view
     , Show view
     , Show msg
     )
  -- => LocalNode
  -- -> SendPort (EngineMsg msg)
  => Chan (EngineMsg msg)
  -> Chan (PubSubMsg view)
  -> WS.PendingConnection
  -> m ()
acceptClientConnection gameMsgChan subscribeChan pendingConnection = do
-- acceptClientConnection node txGameMsg txSubscribe pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $ do
         logInfoN "P: New connection received."
         liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> putStrLn "P: Leaves" )
  liftIO $ WS.forkPingThread connection 30 -- ping the client every 30s
  -- liftIO . runProcess node $ do
  do
    -- (txToPlayer, rxFromBroadcaster) <- newChan
    broacasterToPlayerChan <- newChan
    let disconnectHandler :: WS.ConnectionException -> IO ()
        disconnectHandler ex = do
          liftIO . putStrLn $ "P: Socket has closed. Unsubscribing: " <> show ex
          writeChan subscribeChan (Unsub broacasterToPlayerChan)
          writeChan gameMsgChan $ Leave (getPlayerId broacasterToPlayerChan)
    _ <- spawnLocal $ receiveFromPlayer broacasterToPlayerChan gameMsgChan disconnectHandler connection
    -- _ <- spawnLocal $ receiveFromPlayer txToPlayer txGameMsg disconnectHandler connection
    sendChan txSubscribe (Sub broacasterToPlayerChan)  -- record subscription
    -- sendChan gameMsgChan $ Join (sendPortId txToPlayer) -- add new player to game
    sendChan gameMsgChan $ Join playerId -- add new player to game
    announceToPlayer connection rxFromBroadcaster disconnectHandler

------------------------------------------------------------
-- Player
------------------------------------------------------------
-- TODO This process could do with some refactoring. The walking-cases are a bad sign.
receiveFromPlayer
  :: (Binary msg, Binary view, Show msg, Show view, FromJSON msg)
  => Chan view
  -> Chan (EngineMsg msg)
  -> (WS.ConnectionException -> IO ())
  -> WS.Connection
  -> IO ()
receiveFromPlayer txToPlayer gameMsgChan disconnectHandler connection = do
  liftIO $ putStrLn "P: LISTENING"
  handle Nothing
  where
    handle :: Maybe UTCTime -> IO ()
    handle lastMessageHandled = do
      raw <- liftIO . Ex.try $ WS.receiveDataMessage connection
      case raw of
        Left ex -> disconnectHandler ex
        Right (WS.Binary _) -> handle lastMessageHandled
        Right (WS.Text text _) -> do
          liftIO . putStrLn $ "P: HEARD: " <> show text
          case Aeson.eitherDecode text of
            Left err -> do
              liftIO . putStrLn $ "Couldn't understand: " <> show text
              liftIO . putStrLn $ "  Error was: " <> show err
              handle lastMessageHandled
            Right msg -> do
              now <- liftIO getCurrentTime
              case lastMessageHandled of
                Nothing -> do
                  sendChan gameMsgChan $ GameMsg (sendPortId txToPlayer) msg
                  handle (Just now)
                Just t ->
                  if addUTCTime timeBetweenPlayerCommands t < now
                    then do
                      sendChan gameMsgChan $ GameMsg (sendPortId txToPlayer) msg
                      handle (Just now)
                    else handle lastMessageHandled

------------------------------------------------------------
-- Rate Limiting
------------------------------------------------------------
class Monad m =>
      MonadNow m where
  currentTime :: m UTCTime

instance MonadNow IO where
  currentTime = getCurrentTime

rateLimit
  :: MonadNow m
  => NominalDiffTime -> m a -> m a
rateLimit interval action =
  forever $ do
    now <- currentTime
    evalStateT loop now
  where
    loop = do
      now <- State.lift currentTime
      lastActionTime <- State.get
      if addUTCTime interval lastActionTime < now
        then do
          State.put now
          State.lift action
        else loop

------------------------------------------------------------
announceToPlayer -- used for disconnecting websocket (?)
  :: (Show view, Binary view, ToJSON view)
  => WS.Connection
  -> Chan view
  -> (WS.ConnectionException -> IO ())
  -> IO ()
announceToPlayer connection rx disconnectHandler = handle
  where
    handle = do
      msg <- receiveChan rx
      sent <- liftIO . Ex.try . WS.sendTextData connection $ Aeson.encode msg
      case sent of
        Left ex -> disconnectHandler ex
        Right _ -> handle

------------------------------------------------------------
-- Broadcaster
------------------------------------------------------------
broadcaster
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => Chan (SocketMsgView err gameView playerView) -> Chan (PubSubMsg (SocketMsgView err gameView playerView)) -> IO ()
broadcaster inboundGame subscriptionRequests = iterateM_ handle Set.empty
  where
    handle subscribers = do
      liftIO . putStrLn $ "B: Subscribers: " <> show subscribers
      mergedPorts <-
        mergePortsBiased [Left <$> subscriptionRequests, Right <$> inboundGame]
      msg <- receiveChan mergedPorts
      case msg of
        Left (Sub subscriber) -> do
          liftIO . putStrLn $ "B: adding: " <> show subscriber
          return (Set.insert subscriber subscribers)
        Left (Unsub subscriber) -> do
          liftIO . putStrLn $ "B: removing: " <> show subscriber
          return (Set.delete subscriber subscribers)
        -- Right state@(gameView, [p1View, p2View]) -> do
        Right (Left e) -> do
          liftIO . putStrLn $ "B: error: " <> show e
          traverse_ (sendErrorToMatchingPort e ) $ Set.toList subscribers
          return subscribers
        Right (Right state) -> do
          liftIO . putStrLn $ "B: Broadcasting: " <> show state <> " to: " <> show subscribers
          -- traverse_ (`sendChan` state) $ Set.toList subscribers
          traverse_ (sendFilteredToMatchingPort state ) $ Set.toList subscribers
          return subscribers

sendErrorToMatchingPort 
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => (err, String) -> Chan (SocketMsgView err gameView playerView) -> IO ()
sendErrorToMatchingPort (e, portId) subscriber
  | portId == show (sendPortId subscriber) = sendChan subscriber (Left (e, portId))
  | otherwise                              = return ()


-- send common info + info related to the player only
sendFilteredToMatchingPort 
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => (gameView, [playerView]) -> Chan (SocketMsgView err gameView playerView) -> IO ()
sendFilteredToMatchingPort (common, [p1, p2]) subscriber 
  | destinationPortId p1 == show (sendPortId subscriber) = sendChan subscriber $ Right (common, [p1])
  | destinationPortId p2 == show (sendPortId subscriber) = sendChan subscriber $ Right (common, [p2])
  | otherwise                                            = return ()

------------------------------------------------------------
-- Game
------------------------------------------------------------
gameProcess
  :: (Binary msg, Binary view, Show msg, Show state)
  => Chan (EngineMsg msg)
  -> Chan view
  -> (EngineMsg msg -> state -> SocketMsgGame err state) -- updateFn
  -> (SocketMsgGame err state -> view)                   -- viewFn
  -> state
  -> IO ()
gameProcess rxGameMsg txGameView updateFn viewFn = iterateM_ handle
  where
    handle game = do
      msg <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let msgRes  = updateFn msg game
          newGame = fromRight game msgRes 
      sendChan txGameView (viewFn msgRes)
      return newGame
