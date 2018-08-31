module Main where
 
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (utf8, hSetBuffering, hSetEncoding, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

import Game

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handleP1, _, _) <- accept sock
    configureSocketHandler handleP1
    hPutStrLn handleP1 "New game. Waiting for your opponent to connect..."
    (handleP2, _, _) <- accept sock
    configureSocketHandler handleP2
    hPutStrLn handleP2 "Game starting..."
    -- forkIO $ play handleP1 handleP2
    forkIO $ initNetworkGame handleP1 handleP2
    sockHandler sock

configureSocketHandler :: Handle -> IO ()
configureSocketHandler handle = do
    -- hSetBuffering handle NoBuffering -- ??
    -- setSocketOption handle NoDelay 1 -- disable nagle (cf. https://www.extrahop.com/company/blog/2016/tcp-nodelay-nagle-quickack-best-practices/) uncomment if responses are too slow
    hSetEncoding handle utf8

data PlayerClient = CPlayer1 | CPlayer2

initNetworkGame :: Handle -> Handle -> IO ()
initNetworkGame h1 h2 = do
  forkIO $ sendToGameAs gh CPlayer1 h1
  forkIO $ sendToGameAs gh CPlayer2 h2
  forkIO $ listenGame gh h1 h2

sendToGameAs :: Handler -> PlayerClient -> Handler -> IO ()
sendToGameAs gh pclient h = do
  message <- hGetLine h
  hPutStrLn gh $ show pclient ++ ": " ++ message
  sendToGameAs gh pclient h

listenGame :: Handler -> Handler -> Handler -> IO ()
listenGame gh h1 h2 = do
  message <- hGetLine gh
