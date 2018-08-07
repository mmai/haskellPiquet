module Data ( Suit(..)
            , Rank(..)
            , Card(..)
            , Deck
            , sortedDeck
            , takeCard
            , follows
            ) where

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq,Enum,Bounded)

instance Show Suit where
  show Clubs = "♧"
  show Diamonds = "♦"
  show Hearts = "♥"
  show Spades = "♤"
  -- show Diamonds = "♢"
  -- show Hearts = "♡"
  -- show Clubs = "♣"
  -- show Spades = "♠"

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

-- This is really a State monad
takeCard :: Deck -> (Maybe Card, Deck)
takeCard []     = (Nothing, [])
takeCard (c:cs) = (Just c, cs)

-- |Does the second card follow the first?
follows :: Card -> Card -> Bool
follows (Card Ace _) _ = False
follows c1 c2          = succ (rank c1) == rank c2 && suit c1 == suit c2
