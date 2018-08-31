{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Game
import qualified Network.GameEngine
import           System.Random

main :: IO ()
main = do
  stdGen <- getStdGen
  Network.GameEngine.runGame
    (Game.mkInitialState stdGen)
    Game.update
    Game.viewG
