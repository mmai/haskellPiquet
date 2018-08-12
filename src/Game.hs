{-# LANGUAGE TemplateHaskell #-}

module Game
    ( play
    , Game
    ) where

import Cards
import Shuffle

import Data.List.Split (splitOn)
import Data.Set.Ordered
import Data.Function
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Control.Applicative
import Rainbow

data Role = Elder | Younger deriving (Show)

data Step = Start 
          | Cut 
          | Deal
          | ExchangeElder
          | ExchangeYounger 
          | DeclarePointElder
          | DeclarePointResponse 
          | DeclareSequenceElder
          | DeclareSequenceResponse
          | DeclareSetElder
          | DeclareSetResponse
          | DeclareCarteRougeElder
          | Play1
          | DeclarePointYounger
          | DeclareSequenceYounger
          | DeclareSetYounger
          | DeclareCarteRougeYounger
          | Play

data Player = Player { _hand :: Hand
                     , _roundPoints :: Int
                     , _gamePoints :: Int
                     , _points :: Int
                     , _name :: String
                     , _role :: Role
                     } deriving (Show)
makeLenses ''Player

data Game = Game { _deck :: Deck
                 , _visible :: Deck
                 , _step :: Step
                 , _player1 :: Player
                 , _player2 :: Player
                 }

makeLenses ''Game

 
play :: IO Game 
play = flip execStateT initialState $
  showDeck >> shuffle >> showDeck
  -- let (hands, tally) = drawHands deck 12 2 
  --     hand1 = hands !! 0
  -- print "Hand:"
  -- print hand1
  -- toChange <- fromList . getCardsAtPos hand1 . fmap read . splitOn "," <$> getLine
  -- -- let toChange = fst $ takeNCards hand1 3
  -- let (changed, tally') = changeCards tally hand1 toChange 
  -- print "New hand:"
  -- print changed

initialState = Game
  { _deck = sortedDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo"
  , _player2 = initialPlayer & name .~ "Juliette"
  }
  where initialPlayer = Player { _hand = noCards
                               , _roundPoints = 0
                               , _gamePoints = 0
                               , _points = 0
                               , _name = "undefined"
                               , _role = Elder
                               }

type GameAction = StateT Game IO ()

shuffle :: GameAction
shuffle = do
  state <- get
  shuffledDeck <- lift $ shuffleIO (state ^. deck)
  deck .= shuffledDeck
-- with arrows :
-- shuffle =  get 
--        >>= (   liftA2 (fmap . flip (set deck)) id (view deck >>> shuffleIO) 
--            >>> lift
--            ) 
--        >>= put



showDeck :: GameAction
showDeck = get >>= (view deck >>> print >>> lift) 

