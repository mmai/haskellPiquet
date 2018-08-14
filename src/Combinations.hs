{-# LANGUAGE TupleSections #-}

module Combinations where

import Control.Arrow
import Data.List
import Data.Set.Ordered
import Data.Foldable (toList)

import Cards

data CombinationType = Point | Sequence | Set deriving (Show, Eq)

data Combination = Combination { combinationType :: CombinationType
                               , cards :: Hand
                               } 

instance Show Combination where
  show c = show (combinationType c) ++ " : " ++ show (cards c)

instance Eq Combination where
  (==) ca@(Combination ta ha ) cb@(Combination tb hb ) = 
    (ta /= tb) || EQ == compare ca cb 

instance Ord Combination where
  compare (Combination ta ha ) (Combination tb hb )
    | ta /= tb       = EQ
    | ta == Sequence = EQ
    | ta == Set      = EQ
    | ta == Point    = if length ha /= length hb 
                          then compare (length ha) (length hb) -- nombre de cartes
                          else                                 -- somme des valeurs des cartes
                            if (length ha == 0) 
                               then EQ
                               -- else compare (maximum (toList ha)) (maximum (toList hb))
                               else compare (sum $ pointValue <$> toList ha) (sum $ pointValue <$> toList hb)

pointValue :: Card -> Int 
pointValue (Card rank _) = case rank of 
                             Ace   -> 11
                             King  -> 10
                             Queen -> 10
                             Jack  -> 10
                             _     -> read $ show rank

getCombinations :: CombinationType -> Hand -> [Combination]
getCombinations Point =   toList 
                     >>> sortOn suit 
                     >>> groupBy (\ca cb -> suit ca == suit cb) 
                     >>> fmap fromList 
                     >>> (Combination Point  <$>)
getCombinations Sequence = const []
getCombinations Set = const []


