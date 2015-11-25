module Problem.P035Spec (main, spec) where

import Test.Hspec
import Problem.P035

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cycles" $ do
    it "returns [1] when n = 1" $ do
      cycles 1 `shouldBe` [1]
    it "returns [11,11] when n = 11" $ do
      cycles 11 `shouldBe` [11, 11]
    it "returns [12,21] when n = 12" $ do
      cycles 12 `shouldBe` [12, 21]
    it "returns [123,231,312] when n = 123" $ do
      cycles 123 `shouldBe` [123, 231, 312]

  describe "allDigitsOdd" $ do
    it "returns True when n = 11" $ do
      allDigitsOdd 11 `shouldBe` True
    it "returns False when n = 12" $ do
      allDigitsOdd 12 `shouldBe` False
    it "returns False when n = 51" $ do
      allDigitsOdd 51 `shouldBe` False
    it "returns True when n = 1379" $ do
      allDigitsOdd 1379 `shouldBe` True
    it "returns False when n = 13579" $ do
      allDigitsOdd 13579 `shouldBe` False

  describe "solve" $ do
    it "returns 13 when n = 100" $ do
      solve 100 `shouldBe` 13
