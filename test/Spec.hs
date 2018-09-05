module Main where

import GameTests (tests)
import CombinationsTests (tests)
import CardsTests (tests)
import CmdLineParserTests (tests)

main :: IO ()
main = do
  CombinationsTests.tests
  CardsTests.tests
  GameTests.tests
  CmdLineParserTests.tests
