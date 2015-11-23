module Problem.P031Spec (main, spec) where

import Test.Hspec
import Problem.P031

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "coins" $ do
    it "is [1,2,5,10,20,50,100,200]" $ do
      coins `shouldBe` [1, 2, 5, 10, 20, 50, 100, 200]

  describe "countPatterns" $ do
    it "returns 0 when (n, cs) = (1, [])" $ do
      countPatterns 1 [] `shouldBe` 0
    it "returns 1 when (n, cs) = (0, [1,2])" $ do
      countPatterns 0 [1, 2] `shouldBe` 1
    it "returns 1 when (n, cs) = (1, [1])" $ do
      countPatterns 1 [1] `shouldBe` 1
    it "returns 1 when (n, cs) = (2, [1])" $ do
      countPatterns 2 [1] `shouldBe` 1
    it "returns 0 when (n, cs) = (3, [2])" $ do
      countPatterns 3 [2] `shouldBe` 0
    it "returns 2 when (n, cs) = (2, [1,2])" $ do
      countPatterns 2 [1, 2] `shouldBe` 2
    it "returns 10 when (n, cs) = (10, [1,2,5])" $ do
      countPatterns 10 [1, 2, 5] `shouldBe` 10
    it "returns 11 when (n, cs) = (10, [1,2,5,10])" $ do
      countPatterns 10 [1, 2, 5, 10] `shouldBe` 11

  describe "solve" $ do
    it "returns 1 when n = 1" $ do
      solve 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      solve 2 `shouldBe` 2
    it "returns 11 when n = 10" $ do
      solve 10 `shouldBe` 11
