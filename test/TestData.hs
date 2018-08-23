module TestData where

import Data.Set.Ordered
import Control.Lens

import Game
import Cards
import Combinations

handElder = fromList [ Card Seven Clubs
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

winningComb = Combination Sequence $ 
        fromList [ Card Seven Clubs
                 , Card Eight Clubs
                 , Card Nine Clubs
                 , Card Ten Clubs
                 , Card Jack Clubs
                 ]

pl = Player { _hand = handElder
            , _isElder = True
            , _leftUntilCarteRouge = handElder
            , _dealPoints = 0
            , _gamePoints = 0
            , _points = 0
            , _name = "test"
            }

testGame = Game { _dealNum = One
                    , _deck = sortedDeck
                    , _visible = fromList []
                    , _step = SetPointsSequenceElder
                    , _player1 = pl
                    , _player2 = pl & isElder .~ False
                    , _pointWinner = Nobody
                    , _pointCombination = Nothing
                    , _sequenceWinner = Elder
                    , _sequenceCombination = Just winningComb
                    , _setWinner = Nobody
                    , _setCombination = Nothing
                    }
