module Cards ( Suit(..)
            , Rank(..)
            , Card(..)
            , Deck
            , Hand
            , sortedDeck
            , drawHands
            , takeCard
            , takeCards
            , follows
            ) where

import Control.Monad.State

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq,Enum,Bounded)

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
          deriving (Eq,Enum,Bounded)

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
                 } deriving (Eq)

instance Show Card where
  show (Card rank suit ) = "[" ++ (show rank) ++ " " ++ (show suit) ++ "]"

type Deck = [Card]
type Hand = [Card]

sortedDeck :: Deck
sortedDeck = [Card rank suit | rank <- [Seven .. Ace],  suit <- [Clubs .. Spades]]

takeCard :: Deck -> (Maybe Card, Deck)
takeCard []     = (Nothing, [])
takeCard (c:cs) = (Just c, cs)

takeCards :: Deck -> Int -> (Hand, Deck)
takeCards d n = splitAt n d

drawCards :: Int -> State Deck Hand
drawCards n = do
  d <- get
  let (hand, newDeck) = takeCards d n
  put newDeck
  return hand

drawHandsST :: Int -> Int -> State Deck [Hand]
drawHandsST ncards nhands = replicateM nhands $ drawCards ncards

drawHands :: Deck -> Int -> Int -> ([Hand], Deck) 
drawHands deck ncards nhands = runState (drawHandsST ncards nhands) deck

-- |Does the second card follow the first?
follows :: Card -> Card -> Bool
follows (Card Ace _) _ = False
follows c1 c2          = succ (rank c1) == rank c2 && suit c1 == suit c2

--------------------- Piquet

data Player = Player { hand :: Hand
                     , roundPoints :: Int
                     , gamePoints :: Int
                     , points :: Int
                     , name :: String
                     } deriving (Show)
