module Problem.P045Spec (main, spec) where

import Test.Hspec
import Problem.P045

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPentagonal" $ do
    it "returns True when n = 1" $ do
      isPentagonal 1 `shouldBe` True
    it "returns False when n = 2" $ do
      isPentagonal 2 `shouldBe` False
    it "returns False when n = 3" $ do
      isPentagonal 3 `shouldBe` False
    it "returns False when n = 4" $ do
      isPentagonal 4 `shouldBe` False
    it "returns True when n = 5" $ do
      isPentagonal 5 `shouldBe` True
    it "returns False when n = 6" $ do
      isPentagonal 6 `shouldBe` False
    it "returns True when n = 12" $ do
      isPentagonal 12 `shouldBe` True

  describe "isTriangle" $ do
    it "returns True when n = 1" $ do
      isTriangle 1 `shouldBe` True
    it "returns False when n = 2" $ do
      isTriangle 2 `shouldBe` False
    it "returns True when n = 3" $ do
      isTriangle 3 `shouldBe` True
    it "returns False when n = 4" $ do
      isTriangle 4 `shouldBe` False
    it "returns False when n = 5" $ do
      isTriangle 5 `shouldBe` False
    it "returns True when n = 6" $ do
      isTriangle 6 `shouldBe` True
    it "returns False when n = 12" $ do
      isTriangle 12 `shouldBe` False

  describe "solve" $ do
    it "returns 1 when n = 0" $ do
      solve 0 `shouldBe` 1
    it "returns 40755 when n = 1" $ do
      solve 1 `shouldBe` 40755
