{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cards ( Suit(..)
            , Rank(..)
            , Card(..)
            , Deck
            , Hand
            , sortedDeck
            , sortByColor
            , sortByRank
            , noCards
            , changeCards
            , drawHands
            , takeNCards
            , getCardsAtPos
            ) where

import Control.Monad.State
import Data.List
import Data.Set.Ordered
import Data.Foldable (toList)
import           Data.Binary hiding (get, put)
import qualified Data.Binary as Bin
import Data.Aeson
import GHC.Generics

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq,Enum,Ord,Bounded, Binary, Generic, FromJSON, ToJSON)

instance Show Suit where
  show Diamonds = "♦"
  show Hearts = "♥"
  show Clubs = "♣"
  show Spades = "♠"
  -- show Spades = "♤"
  -- show Clubs = "♧"
  -- show Diamonds = "♢"
  -- show Hearts = "♡"

data Rank = Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq,Enum,Ord,Bounded, Binary, Generic, FromJSON, ToJSON)

instance Show Rank where
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card { rank :: Rank
                 , suit  :: Suit
                 } deriving (Eq, Ord, Binary, Generic, FromJSON, ToJSON)

sortByColor :: Hand -> Hand
sortByColor = fromList . (sortBy compareByColorThenRank) . toList where
  compareByColorThenRank a b
    | a == b            =  EQ
    | suit a == suit b  =  compare (rank a) (rank b)
    | otherwise         =  compare (suit a) (suit b)

sortByRank :: Hand -> Hand
sortByRank = fromList . (sortBy compareByRankThenColor) . toList where
  compareByRankThenColor a b
    | a == b            =  EQ
    | rank a == rank b  =  compare (suit a) (suit b)
    | otherwise         =  compare (rank a) (rank b)

instance Show Card where
  show (Card rank suit ) = "[" ++ show rank ++ " " ++ show suit ++ "]"

instance (Ord a, Binary a) => Binary (OSet a) where
    put s = Bin.put (size s) <> mapM_ Bin.put (toList s)
    get   = liftM fromList Bin.get

instance (Ord a, ToJSON a) => ToJSON (OSet a) where
    toJSON = toJSON . toList

instance (Ord a, FromJSON a) => FromJSON (OSet a) where
    parseJSON = fmap fromList . parseJSON

type Deck = OSet Card
type Hand = OSet Card

sortedDeck :: Deck
sortedDeck = fromList [Card rank suit | rank <- [Seven .. Ace],  suit <- [Clubs .. Spades]]

noCards :: OSet Card
noCards = fromList []

---- Drawings

takeNCards :: Deck -> Int -> (Hand, Deck)
takeNCards d n = let (lhand, ldeck) = splitAt n (toList d)
                  in (sortByColor (fromList lhand), fromList ldeck)

getCardsAtPos :: Hand -> [Int] -> [Card]
getCardsAtPos hand indices = (toList hand !!) <$> indices

drawCards :: Int -> State Deck Hand
drawCards n = do
  d <- get
  let (hand, newDeck) = takeNCards d n
  put newDeck
  return hand

drawHandsSt :: Int -> Int -> State Deck [Hand]
drawHandsSt ncards nhands = replicateM nhands $ drawCards ncards

drawHands :: Deck -> Int -> Int -> ([Hand], Deck) 
drawHands deck ncards nhands = runState (drawHandsSt ncards nhands) deck

-- Remove cards from a hand and replace them by cards from the deck
-- only remove cards really existing in the hand
changeCards :: Deck -> Hand -> OSet Card -> (Hand, Deck)
changeCards deck hand cardsToRemove = 
  let hand' = hand Data.Set.Ordered.\\ cardsToRemove
      (drawnCards, newDeck) = takeNCards deck (size hand - size hand') 
      newHand = hand' <>| drawnCards
   in (newHand, newDeck)

