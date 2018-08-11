module Cards ( Suit(..)
            , Rank(..)
            , Card(..)
            , Deck
            , Hand
            , sortedDeck
            , noCards
            , changeCards
            , drawHands
            , takeNCards
            , getCardsAtPos
            , follows
            ) where

import Control.Monad.State
import Data.Set.Ordered
import Data.Foldable (toList)

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq,Enum,Ord,Bounded)

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
          deriving (Eq,Enum,Ord,Bounded)

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
                 } deriving (Eq, Ord)

instance Show Card where
  show (Card rank suit ) = "[" ++ (show rank) ++ " " ++ (show suit) ++ "]"

type Deck = OSet Card
type Hand = OSet Card

sortedDeck :: Deck
sortedDeck = fromList [Card rank suit | rank <- [Seven .. Ace],  suit <- [Clubs .. Spades]]

noCards :: OSet Card
noCards = fromList []

---- Drawings

takeNCards :: Deck -> Int -> (Hand, Deck)
takeNCards d n = let (lhand, ldeck) = splitAt n (toList d)
                 in (fromList lhand, fromList ldeck)

getCardsAtPos :: Hand -> [Int] -> [Card]
getCardsAtPos hand indices = (toList hand !!) <$> indices

drawCards :: Int -> State Deck Hand
drawCards n = do
  d <- get
  let (hand, newDeck) = takeNCards d n
  put newDeck
  return hand

drawHandsST :: Int -> Int -> State Deck [Hand]
drawHandsST ncards nhands = replicateM nhands $ drawCards ncards

drawHands :: Deck -> Int -> Int -> ([Hand], Deck) 
drawHands deck ncards nhands = runState (drawHandsST ncards nhands) deck

-- Remove cards from a hand and replace them by cards from the deck
-- only remove cards really existing in the hand
changeCards :: Deck -> Hand -> OSet Card -> (Hand, Deck)
changeCards deck hand cardsToRemove = 
  let hand' = hand \\ cardsToRemove
      (drawnCards, newDeck) = takeNCards deck (size hand - size hand') 
      newHand = hand' <>| drawnCards
   in (newHand, newDeck)

---- Combinations

combinationPoints :: Hand -> (Int, Int)
combinationPoints h = (5, 30)

----

-- |Does the second card follow the first?
follows :: Card -> Card -> Bool
follows (Card Ace _) _ = False
follows c1 c2          = succ (rank c1) == rank c2 && suit c1 == suit c2

