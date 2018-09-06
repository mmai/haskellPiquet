{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Combinations where

import Control.Arrow
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Binary hiding (get, put)
import Data.Aeson
import Data.Set.Ordered hiding (filter, empty, null)
import Data.Foldable (toList)
import GHC.Generics

import Cards

data CombinationType = Point | Sequence | Set
          deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

data Combination = Combination { combinationType :: CombinationType
                               , cards :: Hand
                               } 
          deriving (Binary, Generic, FromJSON, ToJSON)

instance Show Combination where
  show c = show (combinationType c) ++ " : " ++ show (cards c)

instance Eq Combination where
  (==) ca@(Combination ta ha ) cb@(Combination tb hb ) = 
    (ta /= tb) || EQ == compare ca cb 

instance Ord Combination where
  compare (Combination ta ha ) (Combination tb hb )
    | ta /= tb               = EQ
    | length ha /= length hb = compare (length ha) (length hb) -- by card count, valid for all combination types
    | ta == Sequence = compare (maximum (toList ha)) (maximum (toList hb))
    | ta == Set      = compare (maximum (toList ha)) (maximum (toList hb))
    | ta == Point    = if length ha == 0
                          then EQ
                          else compare (sum $ pointValue <$> toList ha) (sum $ pointValue <$> toList hb)

-- This is used in the first part of the declaration
compareLength :: Combination -> Combination -> Ordering
compareLength ca cb = compare (length $ cards ca) (length $ cards cb)

showDeclaration :: Combination -> String
showDeclaration (Combination Point cHand) = "Point of " ++ show (length cHand)
showDeclaration (Combination Set cHand) = case length cHand of
                                            3 -> "Trio"
                                            4 -> "Quatorze"
showDeclaration (Combination Sequence cHand) = case length cHand of
                                                 3 -> "Tierce"
                                                 4 -> "Quart"
                                                 5 -> "Cinquième"
                                                 6 -> "Sixième"
                                                 7 -> "Septième"
                                                 8 -> "Huitième"

showDeclarationComplete :: Combination -> String
showDeclarationComplete c@(Combination Point cHand) = showDeclaration c ++ " totaling " ++ show (sum $ pointValue <$> toList cHand)
showDeclarationComplete c@(Combination Sequence cHand) = showDeclaration c ++ " to " ++ show (maximum (toList cHand))
showDeclarationComplete c@(Combination Set cHand) = showDeclaration c ++ " of " ++ show (maximum (toList cHand))
  
showMaybeDeclaration :: Maybe Combination -> String
showMaybeDeclaration mComb = fromMaybe "Nothing" (showDeclaration <$> mComb)

showDeclarations :: [Combination] -> String
showDeclarations = fmap (liftA2 (\a b -> a ++ "(" ++ b ++ ")") show showDeclaration )
  >>> ("Nothing" : )
  >>> intersperse ", "
  >>> concat

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
                         -- >>> concatMap subsequences >>> filter (not . null) 
                         >>> fmap fromList 
                         >>> (Combination Point  <$>)
getCombinations Sequence = sortByColor
                       >>> toList
                       >>> foldl addIfNextInSequence []
                       >>> filter (\cards -> length cards > 2)
                       >>> fmap reverse -- because of foldl
                       >>> fmap fromList 
                       >>> (Combination Sequence  <$>)
getCombinations Set =   sortByRank
                     >>> toList
                     >>> groupBy (\ca cb -> rank ca  == rank cb) 
                     >>> filter (\cards -> rank (head cards) > Nine && length cards > 2)
                     >>> fmap fromList 
                     >>> (Combination Set  <$>)

addIfNextInSequence :: [[Card]] -> Card -> [[Card]]
addIfNextInSequence [] c = [[c]]
addIfNextInSequence [[]] c = [[c]]
addIfNextInSequence acc@(current@(prev:_):rest) c = 
  if (rank prev /= maxBound) && succ (rank prev ) == rank c 
     then (c:current):rest
     -- then (c:current):acc -- we keep subsequences
     else [c]:acc

getSmallerCombinations :: Maybe Combination -> [Combination] -> [Combination]
getSmallerCombinations Nothing = const []
getSmallerCombinations (Just comb) = filter (< comb)

isCarteBlanche :: Hand -> Bool
isCarteBlanche h = filter (\c -> rank c `elem` [King, Queen, Jack]) (toList h) == empty
