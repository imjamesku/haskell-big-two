module Util (updateListAt) where

updateListAt :: Int -> a -> [a] -> [a]
updateListAt idx newVal list
  | idx < 0 || idx >= length list = error "Index out of bounds"
  | otherwise = take idx list ++ [newVal] ++ drop (idx + 1) list