module Game
    ( play
    ) where

import Data
import Shuffle
import Control.Monad.State

play :: IO ()
play = do
  deck <- shuffleIO sortedDeck
  let ((hand1, hand2), tally) = runState (drawHands 12) $ deck
  print hand1
  print hand2
  print tally
  -- maybe (return ()) print $ firstCard deck

drawHands :: Int -> State Deck (Hand, Hand)
drawHands n = do 
  hand1' <- drawCards n
  hand2' <- drawCards n
  return (hand1', hand2')

firstCard :: Deck -> Maybe Card
firstCard d = fst $ takeCard d
