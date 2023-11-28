module BigTwoGame (nextGameState, PlayerAction (..), GameState (..), UpdateGameStateResult (..)) where

import BigTwoCore (Hand, compareCombinations, createCombination, removeHand)
import Util (updateListAt)

-- import BigTwoCore ()
data GameState = GameState {hands :: [Hand], lastHand :: Maybe Hand, turn :: Int, passCount :: Int} deriving (Eq, Show)

data UpdateGameStateResult = UpdateGameStateResult {newGameState :: GameState, message :: String} deriving (Eq, Show)

data PlayerAction = Play Hand | Pass deriving (Eq, Show)

nextGameState :: PlayerAction -> GameState -> UpdateGameStateResult
nextGameState action state = case action of
  Play hand -> handlePlay hand state
  Pass -> handlePass state

incrementTurn :: Int -> Int
incrementTurn turn = (turn + 1) `mod` 4

handlePass :: GameState -> UpdateGameStateResult
handlePass state =
  let newPassCount = passCount state + 1
      nextTurn = incrementTurn $ turn state
   in if newPassCount == 3
        then
          let newGameState = GameState {hands = hands state, lastHand = Nothing, turn = nextTurn, passCount = 0}
           in UpdateGameStateResult {newGameState = newGameState, message = "Player " ++ show (turn state) ++ " passed. Player " ++ show nextTurn ++ "'s turn"}
        else UpdateGameStateResult {newGameState = state {turn = nextTurn, passCount = newPassCount}, message = "Player " ++ show (turn state) ++ " passed. Player " ++ show nextTurn ++ "'s turn"}

handlePlay :: Hand -> GameState -> UpdateGameStateResult
handlePlay selected state =
  let currentPlayerHand = hands state !! turn state
      remaining = removeHand selected currentPlayerHand
      combination = createCombination selected
      lastHandCombination = lastHand state >>= createCombination
   in case (combination, lastHandCombination) of
        (Nothing, _) -> UpdateGameStateResult {newGameState = state, message = "Invalid combination"}
        (_, Nothing) ->
          let newHands = updateListAt (turn state) remaining (hands state)
              newGameState = GameState {hands = newHands, lastHand = Just selected, turn = incrementTurn $ turn state, passCount = 0}
           in UpdateGameStateResult {newGameState = newGameState, message = "Player " ++ show (turn state) ++ " played " ++ show combination}
        (Just c, Just lh) ->
          let comparison = compareCombinations c lh
           in case comparison of
                Nothing -> UpdateGameStateResult {newGameState = state, message = "Invalid combination"}
                Just GT ->
                  let newHands = updateListAt (turn state) remaining (hands state)
                      newGameState = GameState {hands = newHands, lastHand = Just selected, turn = incrementTurn $ turn state, passCount = 0}
                   in UpdateGameStateResult {newGameState = newGameState, message = "Player " ++ show (turn state) ++ " played " ++ show c}
                Just LT -> UpdateGameStateResult {newGameState = state, message = "Your combination is smaller than the last hand"}
                Just EQ -> UpdateGameStateResult {newGameState = state, message = "Your combination is smaller than the last hand"}
