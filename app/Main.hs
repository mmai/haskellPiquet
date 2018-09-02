{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Server
import qualified Game
import qualified Network.GameEngine
import           System.Random

main :: IO ()
main = do
  stdGen <- getStdGen
  Network.GameEngine.runGame
    (Game.mkInitialState stdGen)
    Server.update
    Server.viewG
