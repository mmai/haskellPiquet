{-# LANGUAGE TupleSections #-}

module Combinations where

import Control.Arrow
import Data.List
import Data.Set.Ordered hiding (filter)
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

getCombinationPoints :: Combination -> Int
getCombinationPoints (Combination Point    h) = length h
getCombinationPoints (Combination Set      h) = if length h == 4 then 14 else 3
getCombinationPoints (Combination Sequence h) = if l > 4 then l + 10 else l where l = length h

getCombinations :: CombinationType -> Hand -> [Combination]
getCombinations Point =  sortByColor
                         >>> toList 
                         >>> groupBy (\ca cb -> suit ca == suit cb) 
                         >>> fmap fromList 
                         >>> (Combination Point  <$>)
getCombinations Sequence = sortByColor
                       >>> toList
                       >>> groupBy (\ca cb -> succ (rank ca ) == rank cb) 
                       >>> filter (\cards -> length cards > 2)
                       >>> fmap fromList 
                       >>> (Combination Sequence  <$>)
getCombinations Set =   sortByRank
                     >>> toList
                     >>> groupBy (\ca cb -> rank ca  == rank cb) 
                     >>> filter (\cards -> rank (head cards) > Nine && length cards > 2)
                     >>> fmap fromList 
                     >>> (Combination Set  <$>)


