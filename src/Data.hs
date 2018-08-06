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
          deriving (Eq,Enum,Show,Bounded)

data Rank = Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq,Enum,Show,Bounded)

data Card = Card { rank :: Rank
                 , suit  :: Suit
                 } deriving (Eq,Show)

type Deck = [Card]

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
