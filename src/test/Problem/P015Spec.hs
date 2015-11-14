module Problem.P015Spec (main, spec) where

import Test.Hspec
import Problem.P015

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fact" $ do
    it "returns 1 when n = 0" $ do
      fact 0 `shouldBe` 1
    it "returns 1 when n = 1" $ do
      fact 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      fact 2 `shouldBe` 2
    it "returns 6 when n = 3" $ do
      fact 3 `shouldBe` 6
    it "returns 24 when n = 4" $ do
      fact 4 `shouldBe` 24

  describe "permutation" $ do
    it "returns 1 when n < r" $ do
      permutation 2 3 `shouldBe` 1
    it "returns 1 when r = 0" $ do
      permutation 3 0 `shouldBe` 1
    it "returns 2 when (n, r) = (2, 1)" $ do
      permutation 2 1 `shouldBe` 2
    it "returns 120 when (n, r) = (5, 4)" $ do
      permutation 5 4 `shouldBe` 120
    it "returns 120 when (n, r) = (6, 3)" $ do
      permutation 6 3 `shouldBe` 120

  describe "combination" $ do
    it "returns 0 when n < r" $ do
      combination 2 3 `shouldBe` 0
    it "returns 1 when r = 0" $ do
      combination 3 0 `shouldBe` 1
    it "returns 2 when (n, r) = (2, 1)" $ do
      combination 2 1 `shouldBe` 2
    it "returns 5 when (n, r) = (5, 4)" $ do
      combination 5 4 `shouldBe` 5
    it "returns 20 when (n, r) = (6, 3)" $ do
      combination 6 3 `shouldBe` 20

  describe "solve" $ do
    it "returns 6 when n = 2" $ do
      solve 2 `shouldBe` 6
    it "returns 184756 when n = 10" $ do
      solve 10 `shouldBe` 184756
