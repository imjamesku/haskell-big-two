module Core (createDeck, shuffle, compareCombinations, deal, TwoCards (..), Combination (..), FiveCards (..), Card (..), Rank (..), Suit (..), isStraightHand, isFullHouse, createCombination) where

import Control.Monad
import Data.Array.IO
import Data.Function (on)
import Data.List (foldl', groupBy, maximumBy, sort)
import Data.List.NonEmpty (group)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import System.Random

data Rank = Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Two deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq, Show, Ord)

data FiveCards = FiveCards Card Card Card Card Card deriving (Eq, Show, Ord)

data TwoCards = TwoCards Card Card deriving (Eq, Show, Ord)

type Deck = [Card]

type Hand = [Card]

data Combination = Single Card | Pair TwoCards | Straight FiveCards | FullHouse FiveCards | FourOfAKind FiveCards | StraightFlush FiveCards deriving (Eq, Show)

createDeck :: Deck
createDeck = [Card r s | r <- [Three .. Two], s <- [Clubs .. Spades]]

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

compareCombinations :: Combination -> Combination -> Maybe Ordering
compareCombinations comb1 comb2 = case (comb1, comb2) of
  (Single c1, Single c2) -> Just $ Prelude.compare c1 c2
  (Pair t1, Pair t2) -> Just $ comparePair t1 t2
  (Straight f1, Straight f2) -> Just $ compareStraight f1 f2
  (FullHouse f1, FullHouse f2) -> Just $ compareFullHouse f1 f2
  --   (FourOfAKind _, FourOfAKind _) -> compareFourOfAKind comb1 comb2
  --   (StraightFlush _, StraightFlush _) -> compareStraightFlush comb1 comb2
  _ -> Nothing

comparePair :: TwoCards -> TwoCards -> Ordering
comparePair (TwoCards c1 c2) (TwoCards c3 c4) = Prelude.compare (max c1 c2) (max c3 c4)

compareStraight :: FiveCards -> FiveCards -> Ordering
compareStraight (FiveCards c1 c2 c3 c4 c5) (FiveCards c6 c7 c8 c9 c10) = Prelude.compare (maximum [c1, c2, c3, c4, c5]) (maximum [c6, c7, c8, c9, c10])

compareFullHouse :: FiveCards -> FiveCards -> Ordering
compareFullHouse f1 f2 = Prelude.compare (findRank f1) (findRank f2)
  where
    findRank :: FiveCards -> Rank
    findRank (FiveCards c1 c2 c3 c4 c5) = findMaxKey . foldl' updateMap Map.empty . map rank $ [c1, c2, c3, c4, c5]
    updateMap :: Map.Map Rank Int -> Rank -> Map.Map Rank Int
    updateMap m x = Map.insertWith (+) x 1 m
    findMaxKey :: (Ord v) => Map.Map k v -> k
    findMaxKey = fst . maximumBy (comparing snd) . Map.toList

createCombination :: Hand -> Maybe Combination
createCombination [card] = Just $ Single card
createCombination hand
  | isPair hand = Just $ Pair $ TwoCards (head hand) (last hand)
  | isStraightHand hand = Just $ Straight $ FiveCards (head hand) (hand !! 1) (hand !! 2) (hand !! 3) (last hand)
  | isFullHouse hand = Just $ FullHouse $ FiveCards (head hand) (hand !! 1) (hand !! 2) (hand !! 3) (last hand)
  | otherwise = Nothing

isPair :: Hand -> Bool
isPair hand = length hand == 2 && (rank . head $ hand) == (rank . last $ hand)

rankValue :: Rank -> [Int]
rankValue Ace = [1, 14]
rankValue r = [fromEnum r + 2] -- Two starts at 2, Three is 3, etc.

isStraight :: [Card] -> Bool
isStraight hand
  | length (group sortedValues) == 5 = isConsecutive sortedValues
  | otherwise = False
  where
    sortedValues = sort $ concatMap (rankValue . rank) hand

isLowAceStraight :: [Card] -> Bool
isLowAceStraight hand = sort ranks == [Two, Three, Four, Five, Ace]
  where
    ranks = map rank hand

isStraightHand :: [Card] -> Bool
isStraightHand hand = isStraight hand || isLowAceStraight hand

isConsecutive :: [Int] -> Bool
isConsecutive [] = True
isConsecutive [_] = True
isConsecutive (x : y : xs) = y == x + 1 && isConsecutive (y : xs)

isFullHouse :: [Card] -> Bool
isFullHouse hand =
  let rankGroups = groupBy ((==) `on` rank) $ sort hand
   in case map length rankGroups of
        [2, 3] -> True
        [3, 2] -> True
        _ -> False
