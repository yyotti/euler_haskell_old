module Problem.P012Spec (main, spec) where

import Test.Hspec
import Problem.P012

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "triangleNumbers" $ do
    it "first 10 terms are [1,3,6,10,15,21,28,36,45,55]" $ do
      take 10 triangleNumbers `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

  describe "factorsCount" $ do
    it "returns 1 when n = 1" $ do
      factorsCount 1 `shouldBe` 1
    it "returns 2 when n = 2" $ do
      factorsCount 2 `shouldBe` 2
    it "returns 2 when n = 3" $ do
      factorsCount 3 `shouldBe` 2
    it "returns 3 when n = 4" $ do
      factorsCount 4 `shouldBe` 3
    it "returns 2 when n = 5" $ do
      factorsCount 5 `shouldBe` 2
    it "returns 4 when n = 6" $ do
      factorsCount 6 `shouldBe` 4
    it "returns 2 when n = 7" $ do
      factorsCount 7 `shouldBe` 2
    it "returns 4 when n = 8" $ do
      factorsCount 8 `shouldBe` 4
    it "returns 3 when n = 9" $ do
      factorsCount 9 `shouldBe` 3
    it "returns 4 when n = 10" $ do
      factorsCount 10 `shouldBe` 4
    it "returns 4 when n = 15" $ do
      factorsCount 15 `shouldBe` 4
    it "returns 6 when n = 28" $ do
      factorsCount 28 `shouldBe` 6

  describe "solve" $ do
    it "returns 28 when n = 5" $ do
      solve 5 `shouldBe` 28
    it "returns 120 when n = 15" $ do
      solve 15 `shouldBe` 120
