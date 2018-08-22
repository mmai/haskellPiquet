module GameTests (tests) where

import Test.Hspec
import Text.Printf (printf)
import Data.Set.Ordered
import Control.Lens
import Control.Monad.State

import TestData

import Game
import Cards
import Combinations

testNextDealNum :: Deal -> Deal -> Spec
testNextDealNum deal ndeal =
  it (printf "should return the next deal for : %s -> %s \n" (show deal) (show ndeal)) $
    nextDealNum deal `shouldBe` ndeal

-- testSetCombinationElderPoints :: Spec
-- testSetCombinationElderPoints = 
--   it "returns the correct number of other combinations" $ (`shouldBe` 4 ) =<< do
--     (^. player1 . dealPoints) <$> execStateT (setCombinationElderPoints Sequence) testGame 

tests = hspec $ do
  describe "nextDeal" $ do 
      testNextDealNum Four Five
      testNextDealNum Six Six
  -- describe "setCombinationElderPoints" $ do
  --     testSetCombinationElderPoints 
