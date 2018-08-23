module CardsTests (tests) where

import Test.Hspec
import Text.Printf (printf)
import Data.Set.Ordered
import Control.Lens
import Control.Monad.State

import TestData

import Cards

testChangeCards :: Spec
testChangeCards =
  it "should return unchanged cards when removing no cards" $ do
    let (newHand, newDeck) = changeCards sortedDeck handElder (fromList [])
    putStrLn $ show newDeck
    -- changeCards sortedDeck handElder (fromList []) `shouldBe` (handElder, sortedDeck)
    (newHand, newDeck) `shouldBe` (handElder, sortedDeck)

tests = hspec $ do
  describe "changeCards" $ do
      testChangeCards 
