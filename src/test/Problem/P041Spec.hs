module Problem.P041Spec (main, spec) where

import Test.Hspec
import Problem.P041

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPandigitalN" $ do
    it "returns True when x = 1" $ do
      isPandigitalN 1 `shouldBe` True
    it "returns False when x = 2" $ do
      isPandigitalN 2 `shouldBe` False
    it "returns True when x = 12" $ do
      isPandigitalN 12 `shouldBe` True
    it "returns False when x = 13" $ do
      isPandigitalN 13 `shouldBe` False
    it "returns True when x = 2143" $ do
      isPandigitalN 2143 `shouldBe` True
    it "returns False when x = 2243" $ do
      isPandigitalN 2243 `shouldBe` False

  describe "solve" $ do
    it "is 7652413" $ do
      solve `shouldBe` 7652413
