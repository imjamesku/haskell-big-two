module Core (createDeck, shuffle) where

import Control.Monad
import Data.Array.IO
import System.Random

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq, Show, Ord)

type Deck = [Card]

type Hand = [Card]

createDeck :: Deck
createDeck = [Card r s | r <- [Ace .. King], s <- [Clubs .. Spades]]

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs

deal :: Deck -> [Hand]
deal deck =
  let (hand1, deck') = splitAt 13 deck
      (hand2, deck'') = splitAt 13 deck'
      (hand3, deck''') = splitAt 13 deck''
      (hand4, _) = splitAt 13 deck'''
   in [hand1, hand2, hand3, hand4]
