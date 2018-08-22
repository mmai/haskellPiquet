module Main where

import GameTests (tests)
import CombinationsTests (tests)

main :: IO ()
main = do
  CombinationsTests.tests
  GameTests.tests
