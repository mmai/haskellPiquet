module Game
    ( play
    ) where

import Data
import Shuffle

play :: IO ()
play = do
  putStrLn "Here is a random card:"
  deck <- shuffleIO sortedDeck
  let hand1 = take 12 deck
  print hand1
  -- maybe (return ()) print $ firstCard deck

firstCard :: Deck -> Maybe Card
firstCard d = fst $ takeCard d
