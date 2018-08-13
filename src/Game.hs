{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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
import System.Random

data Role = Elder | Younger deriving (Show)

data Deal = One | Two | Three | Four | Five | Six deriving (Bounded, Eq, Enum, Show)

data Step = Start 
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
          deriving (Enum, Show)

data Player = Player { _hand :: Hand
                     , _roundPoints :: Int
                     , _gamePoints :: Int
                     , _points :: Int
                     , _name :: String
                     } deriving (Show)
makeLenses ''Player

data Game = Game { _deck :: Deck
                 , _visible :: Deck
                 , _step :: Step
                 , _player1 :: Player
                 , _player2 :: Player
                 , _elderIsPlayer1 :: Bool
                 , _dealNum :: Deal
                 } deriving (Show)

makeLenses ''Game

 
play :: IO Game 
play = flip execStateT initialState $
  setElder
 >> replicateM_ 6 playDeal

setElder :: GameAction
setElder = lift (randomRIO (True, False)) >>= assign elderIsPlayer1

playDeal :: GameAction
playDeal =  shuffle 
         -- >> get >>= (dealNum >>> show >>> (++ "---------") >>> print >>> lift)
         >> showDealNum
         >> deal
         >> showGame
         >> showDeck
         >> exchangeForElder >> step .= succ ExchangeElder
         >> exchangeForYounger >> step .= succ ExchangeYounger
         >> setNextDealNum
         >> showGame

setNextDealNum :: GameAction
setNextDealNum = do
  game <- get
  dealNum %= if maxBound == (game ^. dealNum) then const maxBound else succ

initialState = Game
  { _deck = sortedDeck
  , _visible = fromList []
  , _step = Start
  , _player1 = initialPlayer & name .~ "Roméo"
  , _player2 = initialPlayer & name .~ "Juliette"
  , _elderIsPlayer1 = True
  , _dealNum = One
  }
  where initialPlayer = Player { _hand = noCards
                               , _roundPoints = 0
                               , _gamePoints = 0
                               , _points = 0
                               , _name = "undefined"
                               }

type GameAction = StateT Game IO ()

shuffle :: GameAction
shuffle = do
  state <- get
  shuffledDeck <- lift $ shuffleIO (state ^. deck)
  deck .= shuffledDeck
-- with arrows :
-- shuffle =  get 
--        >>= (   liftA2 (fmap . flip (set deck)) id (   view deck 
--                                                   >>> shuffleIO
--                                                   ) 
--            >>> lift
--            ) 
--        >>= put

deal :: GameAction
deal = do
  game <- get
  let (hands, stock) = drawHands (game ^. deck) 12 2 
  player1 . hand .= hands !! 0
  player2 . hand .= hands !! 1
  deck .= stock
  step .= succ Deal 

getElderLens :: Game -> Lens' Game Player
getElderLens game = if (game ^. elderIsPlayer1) then player1 else player2

getYoungerLens :: Game -> Lens' Game Player
getYoungerLens game = if (game ^. elderIsPlayer1) then player2 else player1

exchangeForElder :: GameAction
-- exchangeForElder = get >>= (getElderLens >>> exchangeForPlayer) -- NOK
exchangeForElder = do
  game <- get
  exchangeForPlayer $ getElderLens game 
  -- exchangeForPlayer . getElderLens $ game -- NOK

exchangeForYounger :: GameAction
exchangeForYounger = do
  game <- get
  exchangeForPlayer $ getYoungerLens game 


exchangeForPlayer :: Lens' Game Player -> GameAction
exchangeForPlayer playerLens = do
  game <- get
  lift getLine >>= (   splitOn ","                                            -- [String]
                   >>> fmap read                                              -- [Int]
                   >>> getCardsAtPos (game ^. playerLens . hand)              -- [Card]
                   >>> fromList                                               -- OSet Card
                   >>> changeCards (game ^. deck) (game ^. playerLens . hand) -- (Hand, Deck)
                   >>> ( ($ game) . (playerLens . hand .~ )) *** (deck .~)    -- (Game, Game -> Game)
                   >>> uncurry (&)                                            -- Game
                   >>> put
                  )

showGame :: GameAction
showGame = get >>= ( print >>> lift ) 

showDeck :: GameAction
showDeck = get >>= (view deck >>> print >>> lift) 

showDealNum :: GameAction
showDealNum = get >>= (view dealNum >>> show >>> ("--------- " ++ ) >>> print >>> lift) 

