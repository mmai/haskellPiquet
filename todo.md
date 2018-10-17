# TODO

## Game engine

receive websocket connection requests
  -> p1 { connection1 }
  -> p2 { connection2 } -> fork gamePlay

gamePlay (p1, p2) = 
  ch1 <- newChan playerMsg
  ch2 <- newChan playerMsg
  chg <- newChan gameState
  fork p1gameconn (ch1, chg)
  fork p2gameconn (ch2, chg)
  fork gameconn (ch1, ch2, chg)

  p1gameconn (ch1, chg) =
  receive connection1 message 
    !isP1turn -> send Conn1 "not your turn"
    isP1turn -> processMessage -> send Conn1 res + newState ; send Conn2 newState

http://hackage.haskell.org/package/websockets-0.12.5.2/docs/Network-WebSockets-Connection.html#t:Connection

  - acceptRequest :: PendingConnection -> IO Connection
  - receiveDataMessage :: Connection -> IO DataMessage
  - send :: Connection -> Message -> IO ()

http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html

  - newChan :: IO (Chan a)
  - writeChan :: Chan a -> a -> IO ()
  - readChan :: Chan a -> IO a
  - dupChan :: Chan a -> IO (Chan a)
