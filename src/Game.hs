module Game
    ( play
    ) where

import Cards
import Shuffle
import Control.Monad.State
import Rainbow

play :: IO ()
play = do
  deck <- shuffleIO sortedDeck
  let (hands, tally) = drawHands deck 12 2 
  print hands
  print tally
