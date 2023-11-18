module CoreSpec (spec) where

import Core (createDeck, shuffle)
import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  describe "Core" $ do
    describe "createDeck" $ do
      it "should have 52 cards" $ do
        length createDeck `shouldBe` 52

    describe "shuffle" $ do
      it "should have 52 cards" $ do
        shuffledDeck <- shuffle createDeck
        length shuffledDeck `shouldBe` 52
      it "should have the same cards" $ do
        shuffledDeck <- shuffle createDeck
        sort shuffledDeck `shouldBe` createDeck
      it "should shuffle cards into different orders" $ do
        shuffledDeck1 <- shuffle createDeck
        shuffledDeck2 <- shuffle createDeck
        shuffledDeck1 `shouldNotBe` shuffledDeck2
