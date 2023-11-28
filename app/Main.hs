module Main where

import BigTwoCore (Hand, compareCombinations, createCombination, createDeck, deal, selectFromHand, shuffle, sortHand)
import Data.List (intercalate)
import Util (updateListAt)

data GameState = GameState {hands :: [Hand], lastHand :: Maybe Hand, turn :: Int, passCount :: Int} deriving (Eq, Show)

initState :: IO GameState
initState = do
  deck <- shuffle createDeck
  let hands = sortHand <$> deal deck
  return GameState {hands = hands, lastHand = Nothing, turn = 0, passCount = 0}

showLastHand :: Maybe Hand -> String
showLastHand Nothing = "None"
showLastHand (Just hand) = show hand

stringToIndices :: String -> [Int]
stringToIndices = map read . words

padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '

generateIdxString :: Int -> String -> String
generateIdxString idx cardString = padRight (length cardString) (show idx)

idxToStr :: Hand -> Int -> String
idxToStr hand idx = generateIdxString idx (show (hand !! idx))

generateIndicesString :: Hand -> String
generateIndicesString hand =
  let middle = intercalate "," $ idxToStr hand <$> [0 .. length hand - 1]
   in "[" ++ middle ++ "]"

-- TODO: check if input is valid
isInputValid :: String -> Bool
isInputValid s = True

updateGameState :: GameState -> IO GameState
updateGameState state = do
  let currentPlayerHand = hands state !! turn state
  putStrLn $ "Player " ++ show (turn state) ++ "'s turn"
  putStrLn $ "Last hand: " ++ showLastHand (lastHand state)
  -- TODO: sort hand
  putStrLn $ "Your hand: " ++ show currentPlayerHand
  putStrLn $ "indices  : " ++ generateIndicesString currentPlayerHand
  putStrLn "Please select a combination to play(indices separated by space). p to pass."
  input <- getLine
  if not $ isInputValid input
    then do
      putStrLn "Invalid input"
      return state
    else handlePlayerInput input state

handlePlayerInput :: String -> GameState -> IO GameState
handlePlayerInput input state = do
  let currentPlayerHand = hands state !! turn state
  if input == "p"
    then do
      putStrLn "Pass"
      let newPassCount = passCount state + 1
      if newPassCount == 3
        then do
          putStrLn "Resetting last hand"
          return GameState {hands = hands state, lastHand = Nothing, turn = (turn state + 1) `mod` 4, passCount = 0}
        else return state {turn = (turn state + 1) `mod` 4, passCount = newPassCount}
    else do
      let indices = stringToIndices input
      let (selected, remaining) = selectFromHand indices currentPlayerHand
      putStrLn $ "Selected: " ++ show selected
      putStrLn $ "Remaining: " ++ show remaining
      handlePlay selected remaining state

handlePlay :: Hand -> Hand -> GameState -> IO GameState
handlePlay selected remaining state = do
  let combination = createCombination selected
  case (combination, lastHand state) of
    (Nothing, _) -> do
      putStrLn "Invalid combination\n"
      return state
    (_, Nothing) ->
      let newHands = updateListAt (turn state) remaining (hands state)
       in return GameState {hands = newHands, lastHand = Just selected, turn = (turn state + 1) `mod` 4, passCount = 0}
    (Just c, Just lh) -> do
      let lastHandCombination = createCombination lh
      case lastHandCombination of
        Nothing -> error "Invalid last hand"
        Just lhc -> do
          let comparison = compareCombinations c lhc
          case comparison of
            Nothing -> do
              putStrLn "Invalid combination\n"
              return state
            Just GT -> do
              let newHands = updateListAt (turn state) remaining (hands state)
              return GameState {hands = newHands, lastHand = Just selected, turn = (turn state + 1) `mod` 4, passCount = 0}
            Just LT -> do
              putStrLn "Invalid combination\n"
              return state
            Just EQ -> do
              putStrLn "Invalid combination\n"
              return state

gameLoop :: GameState -> IO ()
gameLoop state = do
  newState <- updateGameState state
  -- check for end conditions
  -- check if any player has no cards left
  let gameEnding = any null (hands newState)
  if gameEnding
    then putStrLn "Game over"
    else do
      gameLoop newState

-- when noCardsLeft $ putStrLn "Game over"
-- when continueGame $ gameLoop newState

startGame :: IO ()
startGame = do
  state <- initState
  gameLoop state

main :: IO ()
main = startGame
