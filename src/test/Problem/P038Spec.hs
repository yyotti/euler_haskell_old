module Problem.P038Spec (main, spec) where

import Test.Hspec
import Problem.P038

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "concatNum" $ do
    it "returns 10 when (a,b) = (1,0)" $ do
      concatNum 1 0 `shouldBe` 10
    it "returns 1 when (a,b) = (0,1)" $ do
      concatNum 0 1 `shouldBe` 1
    it "returns 12 when (a,b) = (1,2)" $ do
      concatNum 1 2 `shouldBe` 12
    it "returns 123 when (a,b) = (12,3)" $ do
      concatNum 12 3 `shouldBe` 123
    it "returns 321 when (a,b) = (3,21)" $ do
      concatNum 3 21 `shouldBe` 321

  describe "concatProducts" $ do
    it "first 10 terms are [1,12,123,...,123456789,12345678910] when n = 1" $ do
      take 10 (concatProducts 1) `shouldBe` [1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789, 12345678910]
    it "first 5 terms are [2,24,246,2468,246810] when n = 2" $ do
      take 5 (concatProducts 2) `shouldBe` [2, 24, 246, 2468, 246810]

  describe "solve" $ do
    it "is 932718654" $ do
      solve `shouldBe` 932718654
