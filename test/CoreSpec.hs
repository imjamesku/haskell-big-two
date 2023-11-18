module CoreSpec (spec) where

import BigTwoCore (Card (Card), Combination (FullHouse, Pair), FiveCards (FiveCards), Rank (Ace, King, Two), Suit (Clubs, Diamonds, Hearts, Spades), TwoCards (TwoCards), compareCombinations, createDeck, shuffle)
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
    describe "compareCombinations" $ do
      describe "compare pairs" $ do
        it "Ace should be greater than King" $ do
          let p1 = Pair $ TwoCards (Card Ace Clubs) (Card Ace Diamonds)
              p2 = Pair $ TwoCards (Card King Clubs) (Card King Diamonds)
          compareCombinations p1 p2 `shouldBe` Just GT
        it "Two should be greater than King" $ do
          let p1 = Pair $ TwoCards (Card Two Clubs) (Card Two Diamonds)
              p2 = Pair $ TwoCards (Card King Clubs) (Card King Diamonds)
          compareCombinations p1 p2 `shouldBe` Just GT
        it "Spades should be greater than Hearts" $ do
          let p1 = Pair $ TwoCards (Card King Clubs) (Card King Hearts)
              p2 = Pair $ TwoCards (Card King Diamonds) (Card King Spades)
          compareCombinations p1 p2 `shouldBe` Just LT
      describe "compare fullhouses" $ do
        it "Ace should be greater than King" $ do
          let p1 = FullHouse $ FiveCards (Card Ace Clubs) (Card Ace Diamonds) (Card Ace Hearts) (Card King Clubs) (Card King Diamonds)
              p2 = FullHouse $ FiveCards (Card King Clubs) (Card King Diamonds) (Card King Hearts) (Card Two Clubs) (Card Two Diamonds)
          compareCombinations p1 p2 `shouldBe` Just GT
