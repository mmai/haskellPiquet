module GameTests (tests) where

import Game
import Cards
import Combinations

import Test.Hspec
import Text.Printf (printf)
import Data.Set.Ordered
import Control.Monad.State

testNextDealNum :: Deal -> Deal -> Spec
testNextDealNum deal ndeal =
  it (printf "should return the next deal for : %s -> %s \n" (show deal) (show ndeal)) $
    nextDealNum deal `shouldBe` ndeal

testSetCombinationElderPoints :: IO Int
testSetCombinationElderPoints = do
  let handElder = fromList [ Card Seven Clubs
                           , Card Eight Clubs
                           , Card Nine Clubs
                           , Card Ten Clubs
                           , Card Jack Clubs
                           , Card Eight Hearts
                           , Card Nine Hearts
                           , Card Ten Hearts
                           , Card Jack Hearts
                           , Card Jack Spades
                           , Card Jack Diamonds
                           , Card Queen Diamonds
                           ]
      winningComb = Combination Point $ 
        fromList [ Card Seven Clubs
                 , Card Eight Clubs
                 , Card Nine Clubs
                 , Card Ten Clubs
                 , Card Jack Clubs
                 ]
      pl = Player { _hand = handElder
                   , _dealPoints = 0
                   , _gamePoints = 0
                   , _points = 0
                   , _name = "test"
                   }
      initialState = Game { _dealNum = One
                          , _deck = sortedDeck
                          , _visible = fromList []
                          , _step = SetPointsPointElder
                          , _player1 = pl
                          , _player2 = pl
                          , _elderIsPlayer1 = True
                          , _pointWinner = Elder
                          , _pointCombination = Just winningComb
                          , _sequenceWinner = Nobody
                          , _sequenceCombination = Nothing
                          , _setWinner = Nobody
                          , _setCombination = Nothing
                          }
  execStateT (setCombinationElderPoints Point) initialState >>= (dealPoints >> return) 

tests = hspec $ do
  describe "nextDeal" $ do 
    testNextDealNum Four Five
    testNextDealNum Six Six
  describe "setCombinationElderPoints" $ do
    testSetCombinationElderPoints `shouldBe` 4 
