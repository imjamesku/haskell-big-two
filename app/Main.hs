module Main where

import BigTwoCore (Hand, createDeck, deal, selectFromHand, shuffle, sortHand)
import BigTwoGame (GameState (..), PlayerAction (Pass, Play), UpdateGameStateResult (..), nextGameState)
import Data.List (intercalate)
import Text.Read (readMaybe)

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

stringToMaybeIndices :: String -> Maybe [Int]
stringToMaybeIndices s = mapM readMaybe (words s)

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

parseInput :: String -> Hand -> Maybe PlayerAction
parseInput s hand = case s of
  "p" -> Just Pass
  _ -> case stringToMaybeIndices s of
    Nothing -> Nothing
    Just indices ->
      let (selected, _) = selectFromHand indices hand
       in if null selected
            then Nothing
            else Just $ Play selected

updateGameState :: GameState -> IO GameState
updateGameState state = do
  let currentPlayerHand = hands state !! turn state
  putStrLn $ "Player " ++ show (turn state) ++ "'s turn"
  putStrLn $ "Last hand: " ++ showLastHand (lastHand state)
  putStrLn $ "Your hand: " ++ show currentPlayerHand
  putStrLn $ "indices  : " ++ generateIndicesString currentPlayerHand
  putStrLn "Please select a combination to play(indices separated by space). p to pass."
  input <- getLine
  case parseInput input currentPlayerHand of
    Nothing -> do
      putStrLn "Invalid input"
      return state
    Just action -> do
      let result = nextGameState action state
      let outputStr = message result
      putStrLn outputStr
      return $ newGameState result

gameLoop :: GameState -> IO ()
gameLoop state = do
  newState <- updateGameState state
  let gameEnding = any null (hands newState)
  if gameEnding
    then putStrLn "Game over"
    else do
      gameLoop newState

startGame :: IO ()
startGame = do
  state <- initState
  gameLoop state

main :: IO ()
main = startGame
