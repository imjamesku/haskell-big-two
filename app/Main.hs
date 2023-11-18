module Main where

import BigTwoCore (Card (..), Combination (..), Deck, FiveCards (..), Hand, Rank (..), Suit (..), TwoCards (..), compareCombinations, createCombination, createDeck, deal, isFullHouse, isStraightHand, shuffle)
import Control.Monad (when)

data GameState = GameState {hands :: [Hand], lastHand :: Hand, turn :: Int, passCount :: Int} deriving (Eq, Show)

initState :: IO GameState
initState = do
  deck <- shuffle createDeck
  return GameState {hands = deal deck, lastHand = [], turn = 0, passCount = 0}

updateGameState :: GameState -> IO GameState
updateGameState state = do
  putStrLn $ "Player " ++ show (turn state) ++ "'s turn"
  input <- getLine
  error "Not implemented"

gameLoop :: GameState -> IO ()
gameLoop state = do
  newState <- updateGameState state
  -- check for end conditions
  let continueGame = False
  when continueGame $ gameLoop newState

startGame :: IO ()
startGame = do
  state <- initState
  gameLoop state

main :: IO ()
main = startGame
