module Main where

import BigTwoCore (Card (..), Combination (..), Deck, FiveCards (..), Hand, Rank (..), Suit (..), TwoCards (..), compareCombinations, createCombination, createDeck, deal, isFullHouse, isStraightHand, shuffle)

data GameState = GameState {hands :: [Hand], lastHand :: Hand, turn :: Int, passCount :: Int} deriving (Eq, Show)

initState :: IO GameState
initState = do
  deck <- shuffle createDeck
  return GameState {hands = deal deck, lastHand = [], turn = 0, passCount = 0}

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
