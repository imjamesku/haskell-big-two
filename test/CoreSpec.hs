module CoreSpec (spec) where

import Core (createDeck)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Core" $ do
    describe "createDeck" $ do
      it "should have 52 cards" $ do
        length createDeck `shouldBe` 52