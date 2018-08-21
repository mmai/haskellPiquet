module GameTests (tests) where

import Game

import Test.Hspec
import Text.Printf (printf)

testNextDealNum :: Deal -> Deal -> Spec
testNextDealNum deal ndeal =
  it (printf "should return the next deal for : %s -> %s \n" (show deal) (show ndeal)) $
    nextDealNum deal `shouldBe` ndeal

tests = hspec $ do
  describe "nextDeal" $ do 
    testNextDealNum Four Five
    testNextDealNum Six Six
