module Game
    ( play
    , Game
    ) where

import Cards
import Shuffle

import Data.List.Split (splitOn)
import Data.Set.Ordered
import Control.Monad.State
import Rainbow

 
play :: IO Game 
play = flip execStateT initialState $
  showDeck >> shuffle >> showDeck
  -- deck <- shuffleIO sortedDeck
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
  { deck = sortedDeck
  , visible = fromList []
  , step = Start
  , player1 = initialPlayer { name = "Roméo" }
  , player2 = initialPlayer { name = "Juliette" }
  }
  where initialPlayer = Player { hand = noCards
                               , roundPoints = 0
                               , gamePoints = 0
                               , points = 0
                               , name = "undefined"
                               , role = Elder
                               }

type GameAction = StateT Game IO ()

shuffle :: GameAction
shuffle = do
  state <- get
  shuffledDeck <- lift $ shuffleIO (deck state)
  put $ state { deck = shuffledDeck }

showDeck :: GameAction
showDeck = do
  state <- get
  lift . print $ deck state

data Role = Elder | Younger deriving (Show)

data Player = Player { hand :: Hand
                     , roundPoints :: Int
                     , gamePoints :: Int
                     , points :: Int
                     , name :: String
                     , role :: Role
                     } deriving (Show)

data Game = Game { deck :: Deck
                 , visible :: Deck
                 , step :: Step
                 , player1 :: Player
                 , player2 :: Player
                 }

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

