module Main where

 
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
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
    hSetBuffering handleP1 NoBuffering
    (handleP2, _, _) <- accept sock
    hSetBuffering handleP2 NoBuffering
    forkIO $ play handleP1 handleP2
    sockHandler sock
