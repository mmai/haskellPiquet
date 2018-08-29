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
    forkIO $ play handleP1 handleP2
    sockHandler sock

configureSocketHandler :: Handle -> IO ()
configureSocketHandler handle = do
    -- hSetBuffering handle NoBuffering -- ??
    -- setSocketOption handle NoDelay 1 -- disable nagle (cf. https://www.extrahop.com/company/blog/2016/tcp-nodelay-nagle-quickack-best-practices/) uncomment if responses are too slow
    hSetEncoding handle utf8
