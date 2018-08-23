module Main where

import GameTests (tests)
import CombinationsTests (tests)
import CardsTests (tests)

main :: IO ()
main = do
  CombinationsTests.tests
  CardsTests.tests
  GameTests.tests
