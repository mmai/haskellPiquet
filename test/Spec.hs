module Main where

import GameTests (tests)

main :: IO ()
main = do
  GameTests.tests
