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
import           Control.Concurrent (forkIO)
import           Control.Monad.Trans (liftIO, lift)

import qualified Control.Exception                        as Ex
import           Control.Monad
import           Control.Monad.Logger -- runStdoutLoggingT 
import           Control.Monad.Loops
import           Control.Monad.State                      (evalStateT)
import qualified Control.Monad.State                      as State
import           Control.Monad.Trans.Resource             (MonadResource,
                                                           allocate,
                                                           runResourceT,
                                                          ReleaseKey)
import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Aeson                               as Aeson
import           Data.Binary
import           Data.Either
import           Data.Foldable
import           Data.Map
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
type Players  = Map PlayerId Connection
type RoomId   = Int
type Rooms    = Map RoomId Players

data NGames = NGames { pending :: Maybe Connection
                     , running :: Rooms
                     }

data EngineMsg msg = Join PlayerId
                   | Leave PlayerId
                   | GameMsg PlayerId msg
                   deriving (Show, Eq, Binary, Generic, ToJSON, FromJSON)

data PubSubMsg view = Sub PlayerId
                    | Unsub PlayerId
                    deriving (Show, Eq, Binary, Generic, ToJSON, FromJSON)

type SocketMsgView err gameView playerView = (Either (err, String) (gameView, [playerView]))
type SocketMsgGame err state = (Either (err, String) state)

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
runGame initialGameState update view = do
  let settings = Warp.setHost "*" . Warp.setPort 8000 $ Warp.defaultSettings
      nGames = NGames Nothing empty
  runStdoutLoggingT $ do
       logInfoN "START"
       -- backend <- liftIO createTransport
       -- node <- liftIO $ newLocalNode backend initRemoteTable
       logInfoN $ sformat ("Starting listener with settings: " % F.string % ":" % F.int) (show $ Warp.getHost settings) (Warp.getPort settings)
  Warp.runSettings settings $ websocketsOr
       WS.defaultConnectionOptions
       (runResourceT .  (flip State.evalStateT nGames) . acceptClientConnection )
       -- (runResourceT .  acceptClientConnection node txGameMsg txSubscription)
       (staticApp $ defaultFileServerSettings "../client/dist") -- TODO : serveur appli web  ??
  runStdoutLoggingT $ logInfoN "END"

acceptClientConnection
  :: ( MonadResource m
     , Binary msg
     , Binary view
     , FromJSON msg
     , ToJSON view
     , Show view
     , Show msg
     )
  => WS.PendingConnection -> State.StateT (PlayersChansMap msg) m ()
acceptClientConnection pendingConnection = do
  (_releaseKey, connection) <- lift $ allocate
        (runStdoutLoggingT $ do
           logInfoN "P: New connection received."
           liftIO $ WS.acceptRequest pendingConnection)
        (\_ -> putStrLn "P: Leaves" )
  liftIO $ WS.forkPingThread connection 30 -- ping the client every 30s
  nGames <- State.get
  let updatedGames = 
        case (pending nGames) of
          Nothing   -> nGames { pending = Just connection }
          Just pendingConn -> nGames { pending = Nothing 
                                     , running = appendGame pendingConn connection (running nGames)
                                     }
  let playerId = getNextPlayerId playersMap
  broacasterToPlayerChan <- liftIO newChan
  let playersMap' = insert playerId broacasterToPlayerChan playersMap
  State.put playersMap'
  let disconnectHandler :: WS.ConnectionException -> IO ()
      disconnectHandler ex = do
        liftIO . putStrLn $ "P: Socket has closed. Unsubscribing: " <> show ex
        writeChan subscribeChan $ Unsub playerId 
        writeChan gameMsgChan $ Leave playerId
  liftIO $ do
    _ <- forkIO $ receiveFromPlayer playerId gameMsgChan disconnectHandler connection
    writeChan subscribeChan $ Sub playerId  -- record subscription
    writeChan gameMsgChan $ Join playerId -- add new player to game
    announceToPlayer connection broacasterToPlayerChan disconnectHandler

appendGame :: Connection -> Connection -> Rooms -> Rooms
appendGame c1 c2 rooms = insert (getNextMapId rooms) newGame rooms where
  newGame = insert 2 c2 $ insert 1 c1 $ empty 

getNextMapId :: Map a -> Int
getNextMapId = (+1) . size

subscriptionBroadcaster
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => Chan (SocketMsgView err gameView playerView) -> IO ()
subscriptionBroadcaster subscriptionRequests = iterateM_ handle Set.empty
  where
    handle subscribers = do
      liftIO . putStrLn $ "B: Subscribers: " <> show subscribers
      msg <- readChan subscriptionRequests
      case msg of
        Sub subscriber -> do
          liftIO . putStrLn $ "B: adding: " <> show subscriber
          return (Set.insert subscriber subscribers)
        Unsub subscriber -> do
          liftIO . putStrLn $ "B: removing: " <> show subscriber
          return (Set.delete subscriber subscribers)


-- getPlayerId :: PlayersChansMap msg -> Chan (EnginMsg msg) -> Maybe PlayerId
-- getPlayerId playersMap chan 
--   | chan `Bitmap.notMemberR` playersMap = Nothing
--   | otherwise                           = Just $ Bitmap.(!>) playersMap chan

-- getPlayerId :: PlayersChansMap msg -> Chan (EnginMsg msg) -> PlayerId
-- getPlayerId = Bitmap.(!>) 

------------------------------------------------------------
-- Player
------------------------------------------------------------
-- TODO This process could do with some refactoring. The walking-cases are a bad sign.
timeBetweenPlayerCommands :: NominalDiffTime
timeBetweenPlayerCommands = 0.1

receiveFromPlayer
  :: (Binary msg, Show msg, FromJSON msg)
  => PlayerId
  -> Chan (EngineMsg msg)
  -> (WS.ConnectionException -> IO ())
  -> WS.Connection
  -> IO ()
receiveFromPlayer playerId gameMsgChan disconnectHandler connection = do
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
                  writeChan gameMsgChan $ GameMsg playerId msg
                  handle (Just now)
                Just t ->
                  if addUTCTime timeBetweenPlayerCommands t < now
                    then do
                      writeChan gameMsgChan $ GameMsg playerId msg
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
      msg <- readChan rx
      sent <- liftIO . Ex.try . WS.sendTextData connection $ Aeson.encode msg
      case sent of
        Left ex -> disconnectHandler ex
        Right _ -> handle

------------------------------------------------------------
-- Broadcasters
------------------------------------------------------------
gameViewBroadcaster
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => Chan (SocketMsgView err gameView playerView) -> IO ()
gameViewBroadcaster inboundGame = iterateM_ handle Set.empty
  where
    handle subscribers = do
      liftIO . putStrLn $ "B: Subscribers: " <> show subscribers
      msg <- readChan inboundGame
      case msg of
        Left e@(err, playerId) -> do
          liftIO . putStrLn $ "B: error: " <> show e
          let playerChan = ! playerId
          -- traverse_ (sendErrorToMatchingPort e ) $ Set.toList subscribers
          
          return subscribers
        Right state -> do
          liftIO . putStrLn $ "B: Broadcasting: " <> show state <> " to: " <> show subscribers
          -- traverse_ (`sendChan` state) $ Set.toList subscribers
          traverse_ (sendFilteredToMatchingPort state ) $ Set.toList subscribers
          return subscribers

-- sendErrorToMatchingPort 
--   :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
--   => (err, String) -> Chan (SocketMsgView err gameView playerView) -> IO ()
-- sendErrorToMatchingPort (e, playerId) subscriber
--   | playerId == show (sendPortId subscriber) = writeChan subscriber (Left (e, playerId))
--   | otherwise                              = return ()


-- send common info + info related to the player only
sendFilteredToMatchingPort 
  :: (Show err, Binary err, Show gameView, Binary gameView, Show playerView, Binary playerView, TaggedView playerView)
  => (gameView, [playerView]) -> Chan (SocketMsgView err gameView playerView) -> IO ()
sendFilteredToMatchingPort (common, [p1, p2]) subscriber 
  | destinationPortId p1 == show (sendPortId subscriber) = writeChan subscriber $ Right (common, [p1])
  | destinationPortId p2 == show (sendPortId subscriber) = writeChan subscriber $ Right (common, [p2])
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
