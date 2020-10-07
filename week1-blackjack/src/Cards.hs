{-# LANGUAGE ScopedTypeVariables #-}

module Cards 
    ( 
        Suit(..)
    ,   Face(..)
    ,   Card(..)
    ,   Deck
    ,   isAce
    ,   isFaceCard
    ,   shuffle
    ,   newDeck
    ,   draw
    ,   putOnTop
    ,   combineDecks
    ) where

import System.Random

data Suit = Hearts 
          | Clubs 
          | Spades 
          | Diamonds deriving (Show,Eq,Enum)

data Face = Ace 
          | Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight 
          | Nine 
          | Ten 
          | Jack 
          | Queen 
          | King deriving (Show,Eq,Enum) 

data Card = Card Suit Face deriving (Eq)

instance Show Card where
  show (Card s f) = (show f) ++ " of "++ (show s)

-- Zero Cost abstraction
newtype Deck = Deck { getDeck :: [Card] } deriving (Eq, Show)

isFaceCard :: Card -> Bool 
isFaceCard (Card _ Jack)  = True
isFaceCard (Card _ Queen) = True
isFaceCard (Card _ King)  = True
isFaceCard _              = False

isAce :: Card -> Bool
isAce (Card _ Ace) = True
isAce _            = False
                    
draw :: Deck -> (Maybe Card, Deck)
draw deck@(Deck [])      = (Nothing, deck)
draw (Deck (card:rest))  = (Just card, Deck rest)

suits :: [Suit]
suits = enumFromTo Hearts Diamonds

faces :: [Face]
faces = enumFromTo Ace King

-- A new deck of cards in order
newDeck :: Deck
newDeck = Deck $ do
  s <- suits
  f <- faces
  return $ Card s f

putOnTop :: Deck -> Card -> Deck
putOnTop (Deck cards) card = Deck $ card : cards

combineDecks :: Deck -> Deck -> Deck
combineDecks (Deck deck1) (Deck deck2) = Deck $ deck1 ++ deck2

-- construct two lists from alternating elements of the input list 
deInterlace :: [a] -> ([a],[a])
deInterlace xs = go xs ([],[])
  where
  go []       res       = res
  go [x]      (ys, zs)  = (x:ys, zs)
  go (x:y:xs) (ys, zs)  = go xs (x:ys, y:zs)

-- Shuffle a deck of cards by splitting it in two, shuffling those individually, and then
-- shuffling those resulting stacks back together
shuffle :: (RandomGen r) => r -> Deck -> Deck
shuffle rnd    deck@(Deck []) = deck 
shuffle rnd    deck@(Deck [a]) = deck 
shuffle rnd    (Deck xs)      = let (left, right)  = deInterlace xs 
                                    (r1, r2)       = split rnd
                                    (Deck left')   = shuffle r1 (Deck $ go r1 left [])
                                    (Deck right')  = shuffle r2 (Deck $ go r2 right [])
                                in  Deck $ left' ++ right'
      where
      go rnd []      os = os
      go rnd (i:is)  os = let (b, rnd') = random rnd
                          in go rnd' is (rest b)
                        where
                        rest b | b         = i:os 
                               | otherwise = os ++ [i]