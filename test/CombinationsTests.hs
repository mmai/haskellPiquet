module CombinationsTests (tests) where

import Test.Hspec
import Text.Printf (printf)
import Data.Set.Ordered
import Control.Lens
import Control.Monad.State

import TestData

import Game
import Cards
import Combinations

testGetCombinations :: Spec
testGetCombinations = 
  it "returns the correct set of combinations" $ 
    getCombinations Sequence handElder `shouldBe` [ 
        Combination Sequence $ fromList [ Card Eight Hearts
                                      , Card Nine Hearts
                                      , Card Ten Hearts
                                      , Card Jack Hearts
                                      ]
      , winningComb
                                                  ]

tests = hspec $ do
  describe "getCombinations" $ do
      testGetCombinations 
